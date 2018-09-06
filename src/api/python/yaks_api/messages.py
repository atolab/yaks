# Copyright (c) 2018 ADLINK Technology Inc.
#
# See the NOTICE file(s) distributed with this work for additional
# information regarding copyright ownership.
#
# This program and the accompanying materials are made available under the
# terms of the LGPL 2.1 which is available at
# https://github.com/atolab/yaks/blob/master/LICENSE
# Contributors: Gabriele Baldoni, ADLINK Technology Inc. - API


'''
This code uses
https://docs.python.org/3.6/library/struct.html
for pack and unpack of the network messages

MESSAGE FORMAT:
LITTLE ENDIAN



7 6 5 4 3 2 1 0
+-+-+-+-+-+-+-+-+ ---------------+
|  MESSAGE CODE |    8bit        |
+-+-+-+-+-+-+-+-+                |
|X|X|X|X|X|A|S|P|    8bit        +--> Header
+-+-+-+-+-+-+-+-+                |
~   Coor. ID    ~  VLE max 64bit |
+---------------+ ---------------+
~   Properties  ~ --> Present if P = 1
+---------------+
~     Body      ~ --> its structure depends on the message code
+---------------+


WIRE MESSAGE:

 7 6 5 4 3 2 1 0
+-+-+-+-+-+-+-+-+
~    Lenght     ~ VLE max 64bit
+-+-+-+-+-+-+-+-+
|  MESSAGE CODE | 8bit
+-+-+-+-+-+-+-+-+
|X|X|X|X|X|A|S|P| 8bit
+-+-+-+-+-+-+-+-+
~    Corr. id   ~ VLE max 64bit
+---------------~
~   Properties  ~ VL
+---------------+
~     Body      ~ VL
+---------------+


'''
import random
import struct
import json
import hexdump
from enum import Enum
from .encoder import VLEEncoder

OPEN = 0x01
CREATE = 0x02
DELETE = 0x03
PUT = 0xA0
PATCH = 0xA1
GET = 0xA2
SUB = 0xB0
UNSUB = 0xB1
EVAL = 0xB2
OK = 0xD0
ERROR = 0xE0


class CreationType(Enum):
    ACCESS = 0
    STORAGE = 1


class Message(object):

    def __init__(self, raw_message=None):
        self.encoder = VLEEncoder()
        self.raw_msg = raw_message
        self.message_code = 0x0
        # 8bit
        self.flags = 0x0
        # 8bit comprises A,S,P
        self.flag_a = 0x0
        # 1bit
        self.flag_s = 0x0
        # 1bit
        self.flag_p = 0x0
        # 1bit
        self.corr_id = 0x0
        # VLE max 64bit
        self.length = 0x0
        # 64bit
        self.properties = []
        # key-value tuple list (string,string)
        self.data = b''
        if self.raw_msg is not None:
            self.unpack()

    def read_vle_field(self, buf, base_p):
        vle_field = []
        data = buf[base_p]
        vle_field.append(data.to_bytes(1, byteorder='big'))
        while data & 0x80:
            base_p = base_p + 1
            data = buf[base_p]
            vle_field.append(data.to_bytes(1, byteorder='big'))
        return self.encoder.decode(vle_field), base_p + 1

    def pack(self):
        header = struct.pack('<BB', self.message_code, self.flags)
        for b in self.encoder.encode(self.corr_id):
            header = header + b

        msg = header
        if self.flag_p:
            num_p = len(self.properties)
            for b in self.encoder.encode(num_p):
                msg = msg + b
            for p in self.properties:
                k = p.get('key').encode() + b'\x00'
                v = p.get('value').encode() + b'\x00'
                len_k = len(k)
                len_v = len(v)

                for b in self.encoder.encode(len_k):
                    msg = msg + b

                fmt = '<{}s'.format(len_k)
                msg = msg + struct.pack(fmt, k)

                for b in self.encoder.encode(len_v):
                    msg = msg + b

                fmt = '<{}s'.format(len_v)
                msg = msg + struct.pack(fmt, v)

        msg = msg + self.data
        self.raw_msg = msg
        return msg

    def pack_for_transport(self):
        vle_length = b''
        for b in self.encoder.encode(len(self.raw_msg)):
            vle_length = vle_length + b
        return vle_length + self.raw_msg

    def unpack(self):
        sub_header = self.raw_msg[0:2]

        msg = struct.unpack("<BB", sub_header)
        self.message_code = msg[0]
        self.flags = msg[1]

        self.flag_a = (self.flags & 0x04) >> 2
        self.flag_s = (self.flags & 0x02) >> 1
        self.flag_p = self.flags & 0x01
        base_p = 2

        self.corr_id, base_p = self.read_vle_field(self.raw_msg, base_p)

        if self.flag_p:
            num_p, base_p = self.read_vle_field(self.raw_msg, base_p)
            for i in range(0, num_p):
                key_length, base_p = self.read_vle_field(self.raw_msg, base_p)
                key_raw = self.raw_msg[base_p:base_p + key_length]
                k = struct.unpack("<{}s".format(key_length),
                                  key_raw)[0].decode()[:-1]
                base_p = base_p + key_length

                value_length, base_p = \
                    self.read_vle_field(self.raw_msg, base_p)
                value_raw = self.raw_msg[base_p:base_p + value_length]
                v = struct.unpack("<{}s".format(value_length),
                                  value_raw)[0].decode()[:-1]
                base_p = base_p + value_length
                self.properties.append({'key': k, 'value': v})

        self.data = self.raw_msg[base_p:base_p + self.length]
        return self

    def set_p(self):
        self.flag_p = 1
        self.flags = self.flags | 0x01

    def unset_p(self):
        self.flag_p = 0
        self.flags = self.flags ^ 0x01

    def set_s(self):
        self.flag_s = 1
        self.flags = self.flags | 0x02

    def unset_s(self):
        self.flag_s = 0
        self.flags = self.flags ^ 0x02

    def set_a(self):
        self.flag_a = 1
        self.flags = self.flags | 0x04

    def unset_a(self):
        self.flag_a = 0
        self.flags = self.flags ^ 0x04

    def set_data(self, data):
        self.data = data

    def remove_data(self):
        self.data = b''

    def get_property(self, key):
        f = [x for x in self.properties if x.get('key') == key]
        if len(f) > 0:
            f[0].get(key)
        return None

    def add_property(self, key, value):
        self.set_p()
        self.properties.append({'key': key, 'value': value})

    def remove_property(self, key):
        f = [x for x in self.properties if x.get('key') == key]
        if len(f) > 0:
            self.properties.remove(f[0])
        if len(self.properties) == 0:
            self.unset_p()

    def dump(self):
        print()
        hexdump.hexdump(self.raw_msg)
        # for i in range(0, len(self.raw_msg)):
        #     if i != 0 and i % 4 == 0:
        #         print()
        #     print('0x%0.2X ' % self.raw_msg[i], end='')
        # print()

    def pprint(self):
        print('\n############ YAKS FE SOCKET MESSAGE ###################')
        print('# CODE: {}'.format(self.message_code))
        print('# CORR.ID: {}'.format(self.corr_id))
        print('# LENGTH: {}'.format(self.length))
        print('# FLAGS: RAW: {} | A:{} S:{} P:{}'.format(
            self.flags, self.flag_a, self.flag_s, self.flag_p))
        if self.flag_p:
            print('# HAS PROPERTIES')
            print('# NUMBER OF PROPERTIES: {}'.format(len(self.properties)))
            for p in self.properties:
                print('#========\n# KEY:{} VALUE: {}'.format(p.get('key'),
                                                             p.get('value')))
            print('#========')
        print('DATA: {}'.format(self.data))
        print('#######################################################')

    def generate_corr_id(self):
        self.corr_id = random.getrandbits(32)


class MessageOpen(Message):
    def __init__(self, username=None, password=None):
        super(MessageOpen, self).__init__()
        self.generate_corr_id()
        self.message_code = OPEN
        if username and password:
            self.add_property('yaks.login', '{}:{}'.format(username, password))


class MessageCreate(Message):
    def __init__(self, type, path, id=None, cache_size=None, config=None,
                 complete=None):
        super(MessageCreate, self).__init__()
        self.message_code = CREATE
        self.generate_corr_id()
        self.add_property('yaks.path', path)
        if id is not None:
            self.add_property('yaks.id', id)
        if type is CreationType.ACCESS:
            self.set_a()
            if cache_size is not None:
                self.add_property('yaks.cache.size', str(cache_size))
        elif type is CreationType.STORAGE:
            self.set_s()
            if config is not None:
                self.add_property('yaks.storage.config', json.dumps(config))
            if complete is not None and complete is True:
                self.add_property('yaks.storage.complete', 'true')


class MessageDelete(Message):
    def __init__(self, id):
        super(MessageDelete, self).__init__()
        self.message_code = DELETE
        self.generate_corr_id()
        self.add_property('yaks.id', id)


class MessagePut(Message):
    def __init__(self, id, key, value):
        super(MessagePut, self).__init__()
        self.message_code = PUT
        self.generate_corr_id()
        self.add_property('yaks.id', id)


class MessagePatch(Message):
    def __init__(self, id, key, value):
        super(MessagePatch, self).__init__()
        self.message_code = PATCH
        self.generate_corr_id()
        self.add_property('yaks.id', id)


class MessageGet(Message):
    def __init__(self, id, key):
        super(MessageGet, self).__init__()
        self.message_code = GET
        self.generate_corr_id()
        self.add_property('yaks.id', id)


class MessageSub(Message):
    def __init__(self, id, key):
        super(MessageSub, self).__init__()
        self.message_code = SUB
        self.generate_corr_id()
        self.add_property('yaks.id', id)


class MessageUnsub(Message):
    def __init__(self, id, subscription_id):
        super(MessageUnsub, self).__init__()
        self.message_code = UNSUB
        self.generate_corr_id()
        self.add_property('yaks.id', id)


class MessageEval(Message):
    def __init__(self, id, key):
        super(MessageEval, self).__init__()
        self.message_code = EVAL
        self.generate_corr_id()
        self.add_property('yaks.id', id)


class MessageOk(Message):
    def __init__(self, id, corr_id):
        super(MessageOk, self).__init__()
        self.message_code = OK
        self.corr_id = corr_id
        self.add_property('yaks.id', id)


class MessageError(Message):
    def __init__(self, id, corr_id):
        super(MessageError, self).__init__()
        self.message_code = ERROR
        self.corr_id = corr_id
        self.add_property('yaks.id', id)
