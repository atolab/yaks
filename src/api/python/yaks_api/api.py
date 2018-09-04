import messages
import socket
import threading
import queue
import errno
import time

BUFFSIZE = 65535

# https://eli.thegreenplace.net/2011/12/27/python-threads-communication-and-stopping


class WorkerThread(threading.Thread):
    def __init__(self, sock, send_q, recv_q):
        super(WorkerThread, self).__init__()
        self.send_q = send_q
        self.recv_q = recv_q
        self.sock = sock
        self.waiting_msgs = {}
        self.sock.setblocking(0)

    def run(self):
        while True:
            time.sleep(0.025)

            queue_ready = not self.send_q.empty()
            socket_ready = True

            if not (queue_ready or socket_ready):
                break

            if socket_ready:
                try:
                    data = self.sock.recv(BUFFSIZE)
                    if data:
                        msg_r = messages.Message(data)
                        print('<Message received')
                        msg_r.pprint()
                        if self.waiting_msgs.get(msg_r.corr_id) is None:
                            print('This message was not expected!')
                        else:
                            self.waiting_msgs.pop(msg_r.corr_id)
                            self.recv_q.put(msg_r)
                except socket.error as e:
                    err = e.args[0]
                    if err == errno.EAGAIN or err == errno.EWOULDBLOCK:
                        pass

            if queue_ready:
                msg_s = self.send_q.get(False)
                self.waiting_msgs.update({msg_s.corr_id: msg_s})
                self.sock.sendall(msg_s.pack())
                print('Message sended>')
                msg_s.pprint()


class Access(object):
    def __init__(self, id,  path, cache_size=0, encoding=None):
        self.id = id

    def put(self, key, value):
        msg_put = messages.MessagePut(self.id, key, value)
        pass

    def delta_put(self, key, value):
        msg_delta = messages.MessagePatch(self.id, key, value)
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
    def __init__(self, path, properties=None):
        pass

    def dispose(self):
        pass


class YAKS(object):
    def __init__(self, server_address, server_port=9876):
        self.send_queue = queue.Queue()
        self.recv_queue = queue.Queue()
        self.address = server_address
        self.port = server_port
        self.accesses = {}
        self.storages = {}
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
        self.lock = threading.Lock()
        self.sock.connect((self.address, self.port))
        open_msg = messages.MessageOpen('username', 'password')
        self.sock.send(open_msg.pack())
        msg = messages.Message(self.sock.recv(BUFFSIZE))
        if msg.message_code == messages.OK and open_msg.corr_id == msg.corr_id:
            self.wt = WorkerThread(self.sock, self.send_queue, self.recv_queue)
            self.wt.start()
        else:
            raise RuntimeError('Server response is wrong')

    def create_access(self, path, cache_size=0, encoding=None):
        id = '12345'
        msg = messages.MessageCreate(messages.CreationType.ACCESS, path, id)
        self.send_queue.put(msg)
        acc = Access(id, path, cache_size, encoding)
        self.accesses.update({id: acc})
        return acc

    def get_accesses(self):
        pass

    def get_access(self, access_id):
        pass

    def create_storage(self, path, properties=None):
        return Storage(path, properties)

    def get_storages(self):
        pass

    def get_storage(self, storage_id):
        pass
