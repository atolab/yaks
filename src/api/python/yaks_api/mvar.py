from threading import Condition, Lock


class MVar(object):

    def __init__(self):
        self.__lock = Lock()
        self.__condition = Condition(lock=self.__lock)
        self.__value = None

    def get(self):
        self.__lock.acquire()
        if self.__value is None:
            self.__condition.wait()
        v = self.__value
        self.__value = None
        self.__condition.notify()
        self.__lock.release()
        return v

    def put(self, value):
        self.__lock.acquire()
        if self.__value is not None:
            self.__condition.wait()
        self.__value = value
        self.__condition.notify()
        self.__lock.release()
