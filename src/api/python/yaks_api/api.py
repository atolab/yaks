import messages
import socket
import threading
import queue
import select
from mvar import MVar
from encoder import VLEEncoder
BUFFSIZE = 1


class SendingThread(threading.Thread):
    def __init__(self, sock, lock, send_q, waiting_msgs):
        super(SendingThread, self).__init__()
        self.lock = lock
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
                self.lock.acquire()
                self.waiting_msgs.update({msg_s.corr_id: (msg_s, var)})
                self.sock.sendall(msg_s.pack())
                self.lock.release()
                print('Message sended>')
                msg_s.pprint()


class ReceivingThread(threading.Thread):
    def __init__(self, sock, lock, waiting_msgs):
        super(ReceivingThread, self).__init__()
        self.lock = lock
        self.sock = sock
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
        return self.encoder.decode(l_vle)

    def run(self):
        self._is_running = True
        while self._is_running:

                i, _, _ = select.select([self.sock], [], [])
                self.lock.acquire()
                length = self.read_length()
                data = self.sock.recv(length)
                msg_r = messages.Message(data)
                print('<Message received')
                msg_r.pprint()
                if self.waiting_msgs.get(msg_r.corr_id) is None:
                    print('This message was not expected!')
                else:
                    _, var = self.waiting_msgs.get(msg_r.corr_id)
                    self.waiting_msgs.pop(msg_r.corr_id)
                    var.put(msg_r)
                self.lock.release()


class Access(object):
    def __init__(self, id, path, cache_size=0, encoding=None):
        self.id = id

    def put(self, key, value):
        #msg_put = messages.MessagePut(self.id, key, value)
        pass

    def delta_put(self, key, value):
        #msg_delta = messages.MessagePatch(self.id, key, value)
        pass

    def remove(self, key):
        pass

    def subscribe(self, key, callback):
        pass

    def get_subscriptions(self):
        pass

    def unsubscribe(self, subscription_id):
        pass

    def get(self, key):
        pass

    def eval(self, key, computation):
        pass

    def close(self):
        pass

    def dispose(self):
        pass


class Storage(object):
    def __init__(self, id, path, properties=None):
        pass

    def dispose(self):
        pass


class YAKS(object):
    def __init__(self, server_address, server_port=9876):
        self.send_queue = queue.Queue()
        self.address = server_address
        self.port = server_port
        self.accesses = {}
        self.storages = {}
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
        self.lock = threading.Lock()
        self.working_set = {}
        self.sock.connect((self.address, self.port))

        self.st = SendingThread(self.sock, self.lock, self.send_queue,
                                self.working_set)
        self.st.start()
        self.rt = ReceivingThread(self.sock, self.lock, self.working_set)
        self.rt.start()

        open_msg = messages.MessageOpen()
        var = MVar()
        self.send_queue.put((open_msg, var))
        msg = var.get()
        if not self.check_msg(msg, open_msg.corr_id):
            raise RuntimeError('Server response is wrong')

    @staticmethod
    def check_msg(msg, corr_id):
        return msg.message_code == messages.OK or corr_id == msg.corr_id

    def close(self):
        self.st.close()
        self.rt.close()

    def create_access(self, path, cache_size=0, encoding=None):
        create_msg = messages.MessageCreate(messages.CreationType.ACCESS,
                                            path)
        var = MVar()
        self.send_queue.put((create_msg, var))
        msg = var.get()
        if self.check_msg(msg, create_msg.corr_id):
            id = msg.get_property('yaks.id')
            acc = Access(id, path, cache_size, encoding)
            self.accesses.update({id: acc})
            return acc
        else:
            return None

    def get_accesses(self):
        return self.accesses

    def get_access(self, access_id):
        return self.accesses.get(access_id)

    def create_storage(self, path, properties=None):
        create_msg = \
            messages.MessageCreate(messages.CreationType.STORAGE, path)
        if properties:
            for k in properties:
                v = properties.get(k)
                create_msg.add_property(k, v)
        var = MVar()
        self.send_queue.put((create_msg, var))
        msg = var.get()
        if self.check_msg(msg, create_msg.corr_id):
            id = msg.get_property('yaks.id')
            sto = Storage(id, path, properties)
            self.storages.update({id: sto})
            return sto
        else:
            return None

    def get_storages(self):
        return self.storages

    def get_storage(self, storage_id):
        return self.storages.get(storage_id)
