import unittest

from library import LibraryName
from machinetypes import Integer, Symbol

class TestLibraryName(unittest.TestCase):
    def test_invalid1(self):
        with self.assertRaises(ValueError):
            LibraryName([Symbol('foo'), Integer(-1)])

    def test_invalid2(self):
        with self.assertRaises(ValueError):
            LibraryName(Symbol('foo'))

    def test_equal(self):
        self.assertEqual(LibraryName([Symbol('foo'), Integer(10)]),
                         LibraryName([Symbol('foo'), Integer(10)]))

    def test_not_equal(self):
        self.assertNotEqual(LibraryName([Symbol('foo'), Integer(10)]),
                            LibraryName([Symbol('foo'), Symbol('10')]))

    def test_hashable(self):
        hash(LibraryName([Symbol('foo'), Integer('0')]))

    def test_mangle_lib_name1(self):
        self.assertEqual(
            '##2-3foo-4spam',
            LibraryName([Symbol('foo'), Symbol('spam')]).mangle())

    def test_mangle_lib_name2(self):
        self.assertEqual(
            '##2-i99-[10]abcdefghij',
            LibraryName([Integer(99), Symbol('abcdefghij')]).mangle())

    def test_mangle_sym1(self):
        self.assertEqual(
            Symbol('##2-3foo-4spam-xyz'),
            LibraryName([Symbol('foo'), Symbol('spam')]).mangle_symbol(Symbol('xyz')))

    def test_mangle_sym2(self):
        self.assertEqual(
            Symbol('##2-i99-[10]abcdefghij-abc'),
            LibraryName([Integer(99), Symbol('abcdefghij')]).mangle_symbol(Symbol('abc')))

    def test_weird1(self):
        lib_name, sym = LibraryName.unmangle_symbol('##0-')
        self.assertEqual(lib_name, LibraryName([]))
        self.assertEqual(sym, Symbol(''))