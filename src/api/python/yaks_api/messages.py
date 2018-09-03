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

           1
 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|            Header             |
+-------------------------------+
|.            Body              |
+-------------------------------+


HEADER:


           1
 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|           MESSAGE CODE        |     16bit
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|              Flags      |A|S|P|     16bit
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                               |
+            Corr. ID           +     64bit
|                               |
+-------------------------------+
|                               |
+            Length             +     64bit
|                               |
+-------------------------------+    TOT SUB-HEADER: 160bit -> 20byte
~          Properties           ~
+-------------------------------+



PROPERTIES

           1
 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|       Properties Number       |   16bit
+-------------------------------+
~          Property             ~
+-------------------------------+
~           Property            ~
+-------------------------------+
~                               ~
+-------------------------------+
~           Property            ~
+-------------------------------+




PROPERTY

           1
 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|          Key-Length           | 16bit
+-------------------------------+
~            Key                ~
+-------------------------------+
|           Value-Length        | 16 bit
+-------------------------------+
~             Value             ~
+-------------------------------+

'''
import random
import struct
import json
import hexdump
from enum import Enum

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
        self.raw_msg = raw_message
        self.message_code = 0x0
        # 16bit
        self.flags = 0x0
        # 16bit comprises A,S,P
        self.flag_a = 0x0
        # 1bit
        self.flag_s = 0x0
        # 1bit
        self.flag_p = 0x0
        # 1bit
        self.corr_id = 0x0
        # 64bit
        self.length = 0x0
        # 64bit
        self.properties = []
        # key-value tuple list (string,string)
        self.data = b''
        if self.raw_msg is not None:
            self.unpack()

    def pack(self):
        sub_header = struct.pack('<HHII', self.message_code, self.flags,
                                 self.corr_id, self.length)
        header = sub_header
        if self.flag_p:
            num_p = len(self.properties)
            header = header + struct.pack('<H', num_p)
            for p in self.properties:
                k = p.get('key').encode() + b'\x00'
                v = p.get('value').encode() + b'\x00'
                len_k = len(k)
                len_v = len(v)
                fmt = '<H{}sH{}s'.format(len_k, len_v)
                header = header + struct.pack(fmt, len_k, k, len_v, v)
        msg = header + self.data
        self.raw_msg = msg
        return msg

    def unpack(self):
        sub_header = self.raw_msg[0:12]

        msg = struct.unpack("<HHII", sub_header)
        self.message_code = msg[0]
        self.flags = msg[1]
        self.corr_id = msg[2]
        self.length = msg[3]

        self.flag_a = self.flags and 0x04 >> 2
        self.flag_s = self.flags and 0x02 >> 1
        self.flag_p = self.flags and 0x01
        base_p = 12
        if self.flag_p:
            prop = self.raw_msg[base_p:base_p + 2]
            r = struct.unpack("<H", prop)
            num_p = r[0]
            base_p = base_p + 2
            for i in range(0, num_p):
                key_length_raw = self.raw_msg[base_p:base_p + 2]
                key_length = struct.unpack("<H", key_length_raw)[0]
                base_p = base_p + 2

                key_raw = self.raw_msg[base_p:base_p + key_length]
                k = struct.unpack("<{}s".format(key_length),
                                  key_raw)[0].decode()[:-1]
                base_p = base_p + key_length

                value_length_raw = self.raw_msg[base_p:base_p + 2]
                value_length = struct.unpack("<H", value_length_raw)[0]
                base_p = base_p + 2

                value_raw = self.raw_msg[base_p:base_p + value_length]
                v = struct.unpack("<{}s".format(value_length),
                                  value_raw)[0].decode()[:-1]
                base_p = base_p + value_length
                self.properties.append({'key': k, 'value': v})

        self.data = self.raw_msg[base_p:base_p + self.length]
        return self

    def set_p(self):
        self.flag_p = 1
        self.flags = self.flags | self.flag_p

    def unset_p(self):
        self.flag_p = 0
        self.flags = self.flags ^ 1

    def set_a(self):
        self.flag_a = 1
        self.flags = self.flags | self.flag_a

    def unset_a(self):
        self.flag_p = 0
        self.flags = self.flags ^ 4

    def set_s(self):
        self.flag_s = 1
        self.flags = self.flags | self.flag_s

    def unset_s(self):
        self.flag_p = 0
        self.flags = self.flags ^ 2

    def set_data(self, data):
        self.data = data

    def remove_data(self):
        self.data = b''

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
        print('# FLAGS: A:{} S:{} P:{}'.format(self.flag_a, self.flag_s,
                                               self.flag_p))
        if self.flag_p:
            print('# HAS PROPERTIES'.format(self.flag_a, self.flag_s,
                                            self.flag_p))
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
    def __init__(self, username, password):
        super(MessageOpen, self).__init__()
        self.generate_corr_id()
        self.message_code = OPEN
        self.add_property('yaks.login', '{}:{}'.format(username, password))
        self.set_p()


class MessageCreate(Message):
    def __init__(self, type, path, id=None, cache_size=None, config=None,
                 complete=None):
        super(MessageCreate, self).__init__()
        self.message_code = CREATE
        self.generate_corr_id()
        self.set_p()
        self.add_property('yaks.path', path)
        if id is not None:
            self.add_property('yaks.id', id)
        if type is CreationType.ACCESS:
            if cache_size is not None:
                self.add_property('yaks.cache.size', str(cache_size))
        elif type is CreationType.STORAGE:
            if config is not None:
                self.add_property('yaks.storage.config', json.dumps(config))
            if complete is not None and complete is True:
                self.add_property('yaks.storage.complete', 'true')


class MessageDelete(Message):
    def __init__(self, id):
        super(MessageDelete, self).__init__()
        self.message_code = DELETE
        self.generate_corr_id()
        self.set_p()
        self.add_property('yaks.id', id)


class MessagePut(Message):
    def __init__(self, id):
        super(MessagePut, self).__init__()
        self.message_code = PUT
        self.generate_corr_id()
        self.set_p()
        self.add_property('yaks.id', id)


class MessagePatch(Message):
    def __init__(self, id):
        super(MessagePatch, self).__init__()
        self.message_code = PATCH
        self.generate_corr_id()
        self.set_p()
        self.add_property('yaks.id', id)


class MessageGet(Message):
    def __init__(self, id):
        super(MessageGet, self).__init__()
        self.message_code = GET
        self.generate_corr_id()
        self.set_p()
        self.add_property('yaks.id', id)


class MessageSub(Message):
    def __init__(self, id):
        super(MessageSub, self).__init__()
        self.message_code = SUB
        self.generate_corr_id()
        self.set_p()
        self.add_property('yaks.id', id)


class MessageUnsub(Message):
    def __init__(self, id):
        super(MessageUnsub, self).__init__()
        self.message_code = UNSUB
        self.generate_corr_id()
        self.set_p()
        self.add_property('yaks.id', id)


class MessageEval(Message):
    def __init__(self, id):
        super(MessageEval, self).__init__()
        self.message_code = EVAL
        self.generate_corr_id()
        self.set_p()
        self.add_property('yaks.id', id)


class MessageOk(Message):
    def __init__(self, id, corr_id):
        super(MessageOk, self).__init__()
        self.message_code = OK
        self.corr_id = corr_id
        self.set_p()
        self.add_property('yaks.id', id)


class MessageError(Message):
    def __init__(self, id, corr_id):
        super(MessageError, self).__init__()
        self.message_code = ERROR
        self.corr_id = corr_id
        self.set_p()
        self.add_property('yaks.id', id)
