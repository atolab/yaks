import unittest
from yaks_api import messages


class MessagesTests(unittest.TestCase):
    def test_pack_unpack(self):
        msg1 = messages.Message()
        msg1.message_code = 0xFE
        msg1.generate_corr_id()

        cid = msg1.corr_id
        msg1.set_p()
        msg1.add_property('key1', 'value1')
        msg1.add_property('key2', 'value2')

        packed_msg1 = msg1.pack()

        msg2 = messages.Message(packed_msg1)

        self.assertEqual(cid, msg2.corr_id)
        self.assertEqual(msg1.message_code, msg2.message_code)
        self.assertEqual(msg1.flag_p, msg2.flag_p)
        self.assertEqual(msg1.properties, msg2.properties)
