import unittest
from yaks_api import messages


class MessagesTests(unittest.TestCase):
    def test_pack_unpack(self):
        msg1 = messages.Message()
        msg1.message_code = 0xFE
        msg1.generate_corr_id()

        cid = msg1.corr_id
        msg1.add_property('key1', 'value1')
        msg1.add_property('key2', 'value2')

        packed_msg1 = msg1.pack()

        msg2 = messages.Message(packed_msg1)

        self.assertEqual(cid, msg2.corr_id)
        self.assertEqual(msg1.message_code, msg2.message_code)
        self.assertEqual(msg1.flags, msg2.flags)
        self.assertEqual(msg1.properties, msg2.properties)

    def test_add_get_string(self):
        msg1 = messages.Message()
        msg1.add_path('test_string_as_payload')
        packed = msg1.pack()
        msg2 = messages.Message(packed)
        self.assertEqual('test_string_as_payload', msg2.get_path())

    def test_add_get_kv(self):
        msg1 = messages.Message()
        msg1.add_key_value('test_key', 'test_value')
        packed = msg1.pack()
        msg2 = messages.Message(packed)
        kv = msg2.get_key_value()
        self.assertEqual('test_key', kv.get('key'))
        self.assertEqual('test_value', kv.get('value'))

    def test_add_get_values(self):
        msg1 = messages.Message()
        v1 = [
            {'key': 'hello', 'value': 'world'},
            {'key': 'another', 'value': 'longvalue'}
        ]
        msg1.add_values(v1)
        packed = msg1.pack()
        msg2 = messages.Message(packed)
        v2 = msg2.get_values()
        self.assertEqual(v1, v2)

    def test_ok_message(self):
        msg1 = messages.MessageOk('1', '123')
        self.assertEqual('1', '1')
