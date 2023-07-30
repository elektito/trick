import io
import unittest

from machinetypes import Bool, Integer, List, Nil, Pair, String, Symbol, Vector
from read import Reader

class TestReader(unittest.TestCase):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.addTypeEqualityFunc(Nil, self._compare_lists)
        self.addTypeEqualityFunc(Pair, self._compare_lists)
        self.addTypeEqualityFunc(Vector, self._compare_vectors)

    def _compare_lists(self, l1, l2, msg=None):
        if self._list_eq(l1, l2):
            return
        if msg is None:
            msg = f'List comparison failed: {l1} != {l2}'
        raise self.failureException(msg)

    def _list_eq(self, l1, l2):
        assert isinstance(l1, (Nil, Pair))
        assert isinstance(l2, (Nil, Pair))

        if isinstance(l1, Nil):
            return isinstance(l2, Nil)
        if isinstance(l2, Nil):
            return isinstance(l1, Nil)


        if isinstance(l1.car, Pair):
            if not isinstance(l2.car, Pair):
                return False
            if not self._list_eq(l1.car, l2.car):
                return False
        else:
            if l1.car != l2.car:
                return False

        if isinstance(l1.cdr, Pair):
            if not isinstance(l2.cdr, Pair):
                return False
            return self._list_eq(l1.cdr, l2.cdr)
        else:
            return l1.cdr == l2.cdr

    def _compare_vectors(self, v1, v2, msg=None):
        if v1.to_python_list() == v2.to_python_list():
            return
        if msg is None:
            msg = f'Vector comparison failed: {v1} != {v2}'
        raise self.failureException(msg)

    def _test(self, string, expected):
        file = io.StringIO(string)
        reader = Reader(file)
        result = reader.read()
        self.assertEqual(result, expected)

    def test_with_leading_whitespace(self):
        self._test('   100', Integer(100))

    def test_with_trialing_whitespace(self):
        self._test('""    ', String(''))

    def test_cramped1(self):
        expected = List.from_list_recursive([Integer(1), [Integer(2), []]])
        self._test('(1(2()))', expected)

    def test_cramped2(self):
        expected = List.from_list_recursive([Integer(1), [Integer(2), [Symbol('quote'), Integer(3)]]])
        self._test("(1(2'3))", expected)
