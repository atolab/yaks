import unittest
from yaks_api import encoder
from random import randint


class MessagesTests(unittest.TestCase):

    def test_encoding(self):
        e = encoder.VLEEncoder()
        i = 268
        res = e.encode(i)
        expected_res = [b'\x8c', b'\x02']
        self.assertEqual(res, expected_res)

    def test_decoding(self):
        e = encoder.VLEEncoder()
        i = [b'\x8c', b'\x02']
        res = e.decode(i)
        expected_res = 268
        self.assertEqual(res, expected_res)

    def test_random_encode_decode(self):
        e = encoder.VLEEncoder()
        i = randint(0, 18446744073709551615)
        encoded = e.encode(i)
        decoded = e.decode(encoded)
        self.assertEqual(i, decoded)
