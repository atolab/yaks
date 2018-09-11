import unittest
from yaks_api import mvar
from threading import Thread
from random import randint
import time


class MVarTests(unittest.TestCase):

    def test_get_put(self):

        def worker(var):
            time.sleep(randint(1, 3))
            var.put(1)

        def worker2(var):
            var.put(3)

        local_var = mvar.MVar()
        Thread(target=worker, args=(local_var,), daemon=True).start()
        res = local_var.get()

        local_var.put(2)
        Thread(target=worker2, args=(local_var,), daemon=True).start()
        res2 = local_var.get()
        res3 = local_var.get()

        self.assertEqual(res, 1)
        self.assertEqual(res2, 2)
        self.assertEqual(res3, 3)
