import io
import unittest

from trick.machinetypes import Integer, Nil, Pair, Symbol, List, String
from trick.read import ReadError, Reader

i0 = Integer(0)
i1 = Integer(1)
i2 = Integer(2)
i3 = Integer(3)
i4 = Integer(4)
i5 = Integer(5)

class TestReader(unittest.TestCase):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.addTypeEqualityFunc(Pair, self._compare_lists)
        self.addTypeEqualityFunc(String, self._compare_strings)

    def _compare_strings(self, s1, s2, msg=None):
        if s1.value == s2.value:
            return
        if msg is None:
            msg = f'String comparison failed: {s1} != {s2}'
        raise self.failureException(msg)

    def _compare_lists(self, l1, l2, msg=None):
        if self._list_eq(l1, l2):
            return
        if msg is None:
            msg = f'List comparison failed: {l1} != {l2}'
        raise self.failureException(msg)

    def _list_eq(self, l1, l2):
        if isinstance(l1, Nil):
            return isinstance(l2, Nil)
        if isinstance(l2, Nil):
            return False

        cur1 = l1
        cur2 = l2
        while isinstance(cur1, Pair) and isinstance(cur2, Pair):
            if isinstance(cur1, Nil):
                return isinstance(cur2, Nil)
            if isinstance(cur2, Nil):
                return isinstance(cur1, Nil)

            if isinstance(cur1.car, List):
                if not isinstance(cur2.car, List):
                    return False
                if not self._list_eq(cur1.car, cur2.car):
                    return False
            else:
                if cur1.car != cur2.car:
                    return False

            cur1 = cur1.cdr
            cur2 = cur2.cdr

        return cur1 == cur2

    def _read(self, text):
        file = io.StringIO(text)
        r = Reader(file)
        return r.read()

    def _test(self, text, expected):
        value = self._read(text)
        self.assertEqual(expected, value)
        return value

    def test_integer_single_digit(self):
        v = self._test('1', Integer(1))
        self.assertEqual(0, v.src_start)
        self.assertEqual(1, v.src_end)

    def test_integer_multiple_digits(self):
        v = self._test('100', Integer(100))
        self.assertEqual(0, v.src_start)
        self.assertEqual(3, v.src_end)

    def test_empty_list(self):
        v = self._test('()', Nil())
        self.assertEqual(0, v.src_start)
        self.assertEqual(2, v.src_end)

    def test_list(self):
        expected = Pair(Symbol('/'), Pair(i1, Pair(i0, Nil())))
        v = self._test('(/ 1 0)', expected)
        self.assertEqual(0, v.src_start)
        self.assertEqual(7, v.src_end)
        ls = v.to_list()
        self.assertEqual(1, ls[0].src_start)
        self.assertEqual(2, ls[0].src_end)
        self.assertEqual(3, ls[1].src_start)
        self.assertEqual(4, ls[1].src_end)
        self.assertEqual(5, ls[2].src_start)
        self.assertEqual(6, ls[2].src_end)

    def test_quote1(self):
        v = self._test("'foo", Pair(Symbol('quote'), Pair(Symbol('foo'), Nil())))
        self.assertEqual(0, v.src_start)
        self.assertEqual(4, v.src_end)

    def test_quote2(self):
        value = Pair(i1, Pair(i2, Nil()))
        expected = Pair(Symbol('quote'), Pair(value, Nil()))
        v = self._test("'(1 2)", expected)
        self.assertEqual(0, v.src_start)
        self.assertEqual(6, v.src_end)

    def test_quasiquote1(self):
        v = self._test('`foo', Pair(Symbol('quasiquote'), Pair(Symbol('foo'), Nil())))
        self.assertEqual(0, v.src_start)
        self.assertEqual(4, v.src_end)

    def test_quasiquote2(self):
        value = Pair(i1, Pair(i2, Nil()))
        expected = Pair(Symbol('quasiquote'), Pair(value, Nil()))
        v = self._test('`(1 2)', expected)
        self.assertEqual(0, v.src_start)
        self.assertEqual(6, v.src_end)

    def test_unquote1(self):
        v = self._test(',foo', Pair(Symbol('unquote'), Pair(Symbol('foo'), Nil())))
        self.assertEqual(0, v.src_start)
        self.assertEqual(4, v.src_end)

    def test_unquote2(self):
        value = Pair(i1, Pair(i2, Nil()))
        expected = Pair(Symbol('unquote'), Pair(value, Nil()))
        v = self._test(',(1 2)', expected)
        self.assertEqual(0, v.src_start)
        self.assertEqual(6, v.src_end)

    def test_unquote_splicing1(self):
        v = self._test(',@foo', Pair(Symbol('unquote-splicing'), Pair(Symbol('foo'), Nil())))
        self.assertEqual(0, v.src_start)
        self.assertEqual(5, v.src_end)

    def test_unquote_splicing2(self):
        value = Pair(i1, Pair(i2, Nil()))
        expected = Pair(Symbol('unquote-splicing'), Pair(value, Nil()))
        v = self._test(',@(1 2)', expected)
        self.assertEqual(0, v.src_start)
        self.assertEqual(7, v.src_end)

    def test_list_not_closed1(self):
        with self.assertRaises(ReadError):
            self._read('(')

    def test_list_not_closed2(self):
        with self.assertRaises(ReadError):
            self._read('(foo (1) 2')

    def test_list_not_closed3(self):
        with self.assertRaises(ReadError):
            self._read('[')

    def test_list_not_closed4(self):
        with self.assertRaises(ReadError):
            self._read('[x y')

    def test_not_opened1(self):
        with self.assertRaises(ReadError):
            self._read(')')

    def test_not_opened2(self):
        with self.assertRaises(ReadError):
            self._read(']')

    def test_large_list(self):
        n = 5000
        text = '(' + ' '.join('1' for _ in range(n)) + ')'
        expected = Nil()
        for _ in range(n):
            expected = Pair(i1, expected)
        self._test(text, expected)

    def test_too_many_items_after_dot(self):
        with self.assertRaises(ReadError):
            self._read('(1 2 . 3 4)')

    def test_no_item_after_dot(self):
        with self.assertRaises(ReadError):
            self._read('(1 2 . )')

    def test_no_item_before_dot(self):
        with self.assertRaises(ReadError):
            self._read('( . 1 )')

    def test_multiple_dots1(self):
        with self.assertRaises(ReadError):
            self._read('( 1 . . 2 )')

    def test_multiple_dots2(self):
        with self.assertRaises(ReadError):
            self._read('( 1 . 2 . 3 )')

    def test_lone_dot(self):
        with self.assertRaises(ReadError):
            self._read('.')

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

    def test_numeric_symbol(self):
        self._test('|99|', Symbol('99'))
