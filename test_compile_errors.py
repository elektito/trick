import io
import unittest

from compile import CompileError, Compiler
from read import Reader

class TestReader(unittest.TestCase):
    def _test_toplevel_error(self, source):
        c = Compiler([])
        with self.assertRaises(CompileError):
            c.compile_toplevel(source)

    def _test_toplevel_noerror(self, source):
        c = Compiler([])
        c.compile_toplevel(source)

    def _test_expr_error(self, source):
        c = Compiler([])
        file = io.StringIO(source)
        r = Reader(file)
        expr = r.read()
        with self.assertRaises(CompileError):
            c.compile_form(expr, [])

    def _test_expr_noerror(self, source):
        c = Compiler([])
        file = io.StringIO(source)
        r = Reader(file)
        expr = r.read()
        c.compile_form(expr, [])

    def test_empty_toplevel_begin(self):
        self._test_toplevel_noerror('(begin)')

    def test_empty_expr_begin(self):
        self._test_expr_error('(begin)')

    def test_empty_body_after_defines(self):
        self._test_toplevel_error('''
        (let ()
          (define x 100)
          (define x 200))
        ''')

    def test_define_in_body(self):
        self._test_toplevel_error('''
        (let ((a 10))
          (define x 100)
          (begin
            (define y 200)
            (display "foo"))
          (define w 400)
          (list x y z w))
        ''')
