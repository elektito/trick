import unittest

from machinetypes import Integer, List, Nil, Pair, Vector, Symbol
from print import SharedPrinter


i1 = Integer(1)
i2 = Integer(2)
i3 = Integer(3)
i4 = Integer(4)
i5 = Integer(5)

specials = {
    Symbol('quote'): "'",
    Symbol('quasiquote'): '`',
    Symbol('unquote'): ',',
    Symbol('unquote-splicing'): ',@',
}


class TestCyclicPrinter(unittest.TestCase):
    def _test(self, value, expected):
        printer = SharedPrinter(value, cycles_only=True)
        s = printer.print()
        self.assertEqual(expected, s)

    def test_empty_list(self):
        self._test(Nil(), '()')

    def test_normal_list(self):
        self._test(Pair(i1, Pair(i2, Nil())), '(1 2)')

    def test_dotted_pair(self):
        self._test(Pair(i1, i2), '(1 . 2)')

    def test_dotted_list(self):
        self._test(Pair(i1, Pair(i2, i3)), '(1 2 . 3)')

    def test_cyclic_list(self):
        p1 = Pair(i1, Nil())
        p3 = Pair(p1, Nil())
        p2 = Pair(i2, p3)
        p1.cdr = p2
        self._test(p1, '#0=(1 2 #0#)')

    def test_cyclic_after_dot_list(self):
        p1 = Pair(i1, Nil())
        p2 = Pair(i2, p1)
        p1.cdr = p2
        self._test(p1, '#0=(1 2 . #0#)')

    def test_shared_sublist(self):
        sublist = List.from_list([i2, i3])
        p = List.from_list([i1, sublist, i4, sublist, i5])
        self._test(p, '(1 (2 3) 4 (2 3) 5)')

    def test_deep_cyclic(self):
        p1 = Pair(i1, Nil())
        dotted = Pair(p1, p1)
        p2 = Pair(i2, Pair(dotted, Nil()))
        p3 = Pair(p2, Pair(i3, Nil()))
        p1.cdr = p3
        self._test(p1, '#0=(1 (2 (#0# . #0#)) 3)')

    def test_shared_cons(self):
        p1 = Pair(i1, i2)
        p2 = Pair(p1, Pair(i3, p1))
        self._test(p2, '((1 . 2) 3 1 . 2)')

    def test_empty_vector(self):
        self._test(Vector([]), '#()')

    def test_normal_vector(self):
        self._test(Vector([1, 2, 3]), '#(1 2 3)')

    def test_cyclic_vector(self):
        vec = Vector([i1, i2, Nil()])
        vec[2] = vec
        self._test(vec, '#0=#(1 2 #0#)')

    def test_cyclic_mixed_vector_and_list1(self):
        vec = Vector([i1, i2, Nil()])
        ls = Pair(vec, Nil())
        vec[2] = ls
        self._test(vec, '#0=#(1 2 (#0#))')

    def test_cyclic_mixed_vector_and_list2(self):
        vec = Vector([Nil()])
        ls = Pair(i1, Pair(i2, Pair(vec, Nil())))
        vec[0] = ls
        self._test(ls, '#0=(1 2 #(#0#))')

    def test_shared_mixed_vector_and_list1(self):
        v1 = Vector([i2])
        v2 = Vector([i1, v1])
        p = Pair(i1, Pair(v2, Pair(v1, Nil())))
        self._test(p, '(1 #(1 #(2)) #(2))')

    def test_shared_mixed_vector_and_list2(self):
        ls1 = Pair(i2, Nil())
        ls2 = Pair(i1, Pair(ls1, Nil()))
        vec = Vector([i1, ls2, ls1])
        self._test(vec, '#(1 (1 (2)) (2))')

    def test_special1(self):
        for long, short in specials.items():
            p = List.from_list([long, Symbol('foo')])
            self._test(p, f'{short}foo')

    def test_special2(self):
        for long, short in specials.items():
            p = List.from_list_recursive([long, [Symbol('foo'), Symbol('bar')]])
            self._test(p, f'{short}(foo bar)')

    def test_special3(self):
        for long, short in specials.items():
            shared = Pair(i1, Pair(i2, Nil()))
            value = Pair(Symbol('foo'), shared)
            ls = Pair(long, Pair(value, Nil()))
            p = Pair(i1, Pair(shared, Pair(ls, Nil())))
            self._test(p, f'(1 (1 2) {short}(foo 1 2))')

    def test_special4(self):
        for long, short in specials.items():
            p = Pair(i1, Nil())
            value = Pair(Symbol('foo'), Pair(p, Nil()))
            p.cdr = Pair(long, Pair(value, Nil()))
            self._test(p, f'#0=(1 {long} (foo #0#))')

    def test_special5(self):
        for long, short in specials.items():
            p = Pair(i1, Nil())
            p.cdr = Pair(Pair(long, Pair(p, Nil())), Nil())
            self._test(p, f'#0=(1 {short}#0#)')

    def test_special6(self):
        for long, short in specials.items():
            p = Pair(Symbol('foo'), Pair(long, Pair(i5, Nil())))
            self._test(p, f'(foo {long} 5)')

    def test_special7(self):
        for long, short in specials.items():
            form = Pair(long, Pair(Symbol('foo'), Nil()))
            p = Pair(Symbol('list'), Pair(form, Pair(form, Nil())))
            self._test(p, f'(list {short}foo {short}foo)')

    def test_special8(self):
        for long, short in specials.items():
            p = Pair(i1, Nil())
            p.cdr = Pair(long, Pair(p, Nil()))
            self._test(p, f'#0=(1 {long} #0#)')
