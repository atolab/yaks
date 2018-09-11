import unittest
from yaks_api import path


class PathTests(unittest.TestCase):

    def test_path_creation(self):
        p = path.Path('//this/is/a/path')
        self.assertEqual(p.path, '//this/is/a/path')

    def test_path_prefix(self):
        p = path.Path('//this/is/a/path/with/a/prefix')
        self.assertEqual(p.is_prefix('//this/is/a/path'), True)
        self.assertEqual(p.is_prefix('//that/is/a/path'), False)

    def test_path_creation_error(self):
        p = '/this/is/a/not/path'
        self.assertRaises(ValueError, path.Path, p)

    def test_path_query(self):
        p = path.Path('//this/is/a/path?with=query&data=somedata')
        q = {'with': 'query', 'data': 'somedata'}
        self.assertEqual(q, p.get_query())

    def test_path_query_complex(self):
        p = path.Path('//this/is/a/path?with=query&data.level2=somedata')
        q = {'with': 'query', 'data': {'level2': 'somedata'}}
        self.assertEqual(q, p.get_query())
