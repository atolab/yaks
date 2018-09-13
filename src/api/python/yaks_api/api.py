import socket
import threading
import queue
import select
import os
from .messages import *
from .mvar import MVar
from .encoder import VLEEncoder
from .logger import APILogger

BUFFSIZE = 1
ver = os.environ.get('YAKS_PYTHON_API_LOGFILE')
if ver is None:
    VERBOSE = False
else:
    VERBOSE = bool(os.environ.get('YAKS_PYTHON_API_VERBOSE'))
logger = APILogger(file_name=os.environ.get('YAKS_PYTHON_API_LOGFILE'),
                   debug_flag=VERBOSE)


class SendingThread(threading.Thread):
    def __init__(self, sock, lock, send_q, waiting_msgs, subscriptions):
        super(SendingThread, self).__init__()
        self.lock = lock
        self.subscriptions = subscriptions
        self.send_q = send_q
        self.sock = sock
        self.waiting_msgs = waiting_msgs
        self._is_running = False
        self.daemon = True

    def close(self):
        self._is_running = False

    def run(self):
        self._is_running = True
        while self._is_running:
                msg_s, var = self.send_q.get()
                logger.info('SendingThread', 'Message from queue')
                logger.debug('SendingThread', 'Message {}'.
                             format(msg_s.pprint()))
                self.lock.acquire()
                self.waiting_msgs.update({msg_s.corr_id: (msg_s, var)})
                try:
                    self.sock.sendall(msg_s.pack_for_transport())
                except OSError as e:
                    if e.errno == 9:
                        logger.error('SendingThread', 'Bad FD')
                    self._is_running = False
                finally:
                    self.lock.release()

                logger.debug('SendingThread', 'Message Sent on Wire\n{}'.
                             format(msg_s.dump_net()))


class ReceivingThread(threading.Thread):
    def __init__(self, sock, lock, send_q, waiting_msgs, subscriptions):
        super(ReceivingThread, self).__init__()
        self.lock = lock
        self.sock = sock
        self.send_q = send_q
        self.subscriptions = subscriptions
        self.waiting_msgs = waiting_msgs
        self._is_running = False
        self.daemon = True
        self.encoder = VLEEncoder()

    def close(self):
        self._is_running = False
        self.lock.acquire()
        self.sock.close()
        self.lock.release()

    def read_length(self):
        l_vle = []
        data = self.sock.recv(BUFFSIZE)

        l_vle.append(data)
        while int.from_bytes(data, byteorder='big', signed=False) & 0x80:
            data = self.sock.recv(BUFFSIZE)
            l_vle.append(data)
        logger.info('ReceivingThread', 'Read VLE {} of {} bytes'.
                    format(l_vle, len(l_vle)))
        return self.encoder.decode(l_vle)

    def run(self):
        self._is_running = True
        while self._is_running:

                try:
                    i, _, _ = select.select([self.sock], [], [])
                except OSError as e:
                    if e.errno == 9:
                        logger.error('ReceivingThread', 'Bad FD')
                    self._is_running = False
                    break

                logger.info('ReceivingThread', 'Socket ready')
                self.lock.acquire()
                length = self.read_length()
                if length > 0:
                    data = self.sock.recv(length)
                    msg_r = Message(data)
                    logger.info('ReceivingThread',
                                'Read from socket {} bytes'.format(length))
                    logger.debug('ReceivingThread', 'Message Received {} \n{}'.
                                 format(msg_r.pprint(), msg_r.dump()))
                    if msg_r.message_code == NOTIFY:
                        sid, kvs = msg_r.get_notification()
                        if sid in self.subscriptions:
                            cbk = self.subscriptions.get(sid)
                            threading.Thread(target=cbk, args=(kvs,),
                                             daemon=True).start()
                            ok = MessageOk(0, msg_r.corr_id)
                            self.send_q.put((ok, MVar()))
                    elif self.waiting_msgs.get(msg_r.corr_id) is None:
                        print('This message was not expected!')
                    else:
                        if msg_r.message_code == ERROR:
                            logger.info('ReceivingThread',
                                        'Got Error on Message {} '
                                        'Error Code: {}'
                                        .format(msg_r.corr_id,
                                                msg_r.get_error()))
                        _, var = self.waiting_msgs.get(msg_r.corr_id)
                        self.waiting_msgs.pop(msg_r.corr_id)
                        var.put(msg_r)
                else:
                    raise \
                        ConnectionError('The server has closed the connection')
                self.lock.release()


class Access(object):
    def __init__(self, queue, id, path, cache_size=0, encoding=RAW, subs={}):
        self.__subscriptions = subs
        self.__send_queue = queue
        self.id = id
        self.path = path
        self.cache_size = cache_size
        self.encoding = encoding

    def put(self, key, value):
        msg_put = MessagePut(self.id, key, value, encoding=self.encoding)
        var = MVar()
        self.__send_queue.put((msg_put, var))
        r = var.get()
        if YAKS.check_msg(r, msg_put.corr_id):
            return True
        return False

    def delta_put(self, key, value):
        msg_delta = MessagePatch(self.id, key, value, encoding=self.encoding)
        var = MVar()
        self.__send_queue.put((msg_delta, var))
        r = var.get()
        if YAKS.check_msg(r, msg_delta.corr_id):
            return True
        return False

    def remove(self, key):
        msg_rm = MessageDelete(self.id, path=key)
        var = MVar()
        self.__send_queue.put((msg_rm, var))
        r = var.get()
        if YAKS.check_msg(r, msg_rm.corr_id):
            return True
        return False

    def subscribe(self, key, callback):
        msg_sub = MessageSub(self.id, key, encoding=self.encoding)
        var = MVar()
        self.__send_queue.put((msg_sub, var))
        r = var.get()
        if YAKS.check_msg(r, msg_sub.corr_id, expected=OK):
            subid = msg_sub.get_property('is.yaks.subscription.id')
            self.__subscriptions.update({subid: callback})
            return subid
        return None

    def get_subscriptions(self):
        return self.__subscriptions

    def unsubscribe(self, subscription_id):
        msg_unsub = MessageUnsub(self.id, subscription_id)
        var = MVar()
        self.__send_queue.put((msg_unsub, var))
        r = var.get()
        if YAKS.check_msg(r, msg_unsub.corr_id, expected=OK):
            subid = msg_unsub.get_property('is.yaks.subscription.id')
            if subid != subscription_id:
                raise RuntimeError('Subscription ID does not match'
                                   ' (local) {} != (net) {}'.
                                   format(subscription_id, subid))
            self.__subscriptions.pop(subscription_id)
            return True
        return False

    def get(self, key):
        msg_get = MessageGet(self.id, key, encoding=self.encoding)
        var = MVar()
        self.__send_queue.put((msg_get, var))
        r = var.get()
        if YAKS.check_msg(r, msg_get.corr_id, expected=VALUES):
                return r.get_values()
        return None

    def eval(self, key, computation):
        raise NotImplementedError('Not yet...')

    def dispose(self):
        var = MVar()
        msg = MessageDelete(self.id, EntityType.ACCESS)
        self.__send_queue.put((msg, var))
        r = var.get()
        if YAKS.check_msg(r, msg.corr_id):
            return True
        return False


class Storage(object):
    def __init__(self, queue, id, path, properties=[]):
        self.__send_queue = queue
        self.id = id
        self.path = path
        self.properties = properties
        logger.info('Storage __init__', 'Created storage {} - {}'.
                    format(self.id, self.path))

    def dispose(self):
        var = MVar()
        msg = MessageDelete(self.id, EntityType.STORAGE)
        self.__send_queue.put((msg, var))
        r = var.get()
        if YAKS.check_msg(r, msg.corr_id):
            return True
        return False


class YAKS(object):
    def __init__(self, server_address, server_port=7887):
        self.subscriptions = {}
        self.send_queue = queue.Queue()
        self.address = server_address
        self.port = server_port
        self.accesses = {}
        self.storages = {}
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
        self.sock.setblocking(1)
        self.lock = threading.Lock()
        self.working_set = {}
        self.sock.connect((self.address, self.port))

        self.st = SendingThread(self.sock, self.lock, self.send_queue,
                                self.working_set, self.subscriptions)
        self.rt = ReceivingThread(self.sock, self.lock, self.send_queue,
                                  self.working_set, self.subscriptions)
        self.st.start()
        self.rt.start()

        open_msg = MessageOpen()
        var = MVar()
        self.send_queue.put((open_msg, var))
        msg = var.get()
        if not self.check_msg(msg, open_msg.corr_id):
            raise RuntimeError('Server response is wrong')

    @staticmethod
    def check_msg(msg, corr_id, expected=OK):
        return msg.message_code == expected or corr_id == msg.corr_id

    def close(self):
        self.st.close()
        self.rt.close()

    def create_access(self, path, cache_size=1024, encoding=RAW):
        create_msg = MessageCreate(EntityType.ACCESS, path)
        var = MVar()
        self.send_queue.put((create_msg, var))
        msg = var.get()
        if self.check_msg(msg, create_msg.corr_id):
            id = msg.get_property('is.yaks.access.id')
            acc = Access(self.send_queue, id, path, cache_size, encoding)
            self.accesses.update({id: acc})
            return acc
        else:
            return None

    def get_accesses(self):
        return self.accesses

    def get_access(self, access_id):
        return self.accesses.get(access_id)

    def create_storage(self, path, properties=None):
        create_msg = MessageCreate(EntityType.STORAGE, path)
        if properties:
            for k in properties:
                v = properties.get(k)
                create_msg.add_property(k, v)
        var = MVar()
        self.send_queue.put((create_msg, var))
        msg = var.get()
        if self.check_msg(msg, create_msg.corr_id):
            id = msg.get_property('is.yaks.storage.id')
            sto = Storage(self.send_queue, id, path, properties)
            self.storages.update({id: sto})
            return sto
        else:
            return None

    def get_storages(self):
        return self.storages

    def get_storage(self, storage_id):
        return self.storages.get(storage_id)
