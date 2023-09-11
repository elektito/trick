import io
import unittest

from trick.compile import Compiler, ToplevelEnvironment
from trick.exceptions import CompileError
from trick.importsets import LibraryImportSet
from trick.libname import LibraryName
from trick.read import Reader

class TestCompiler(unittest.TestCase):
    def setUp(self):
        self.env = ToplevelEnvironment()

        lib_name = LibraryName.create('trick', 'core')
        self.env.add_import(LibraryImportSet(lib_name))

    def _test_toplevel_error(self, source, error=None):
        c = Compiler([])
        with self.assertRaises(CompileError) as e:
            c.compile_program(source, env=self.env)
        if error:
            self.assertIn(error, str(e.exception))

    def _test_toplevel_noerror(self, source):
        c = Compiler([])
        c.compile_program(source, env=self.env)

    def _test_expr_error(self, source, error=None):
        c = Compiler([])
        file = io.StringIO(source)
        r = Reader(file)
        expr = r.read()
        with self.assertRaises(CompileError) as e:
            c.compile_form(expr, self.env, tail=False)
        if error:
            self.assertIn(error, str(e.exception))

    def _test_expr_noerror(self, source):
        c = Compiler([])
        file = io.StringIO(source)
        r = Reader(file)
        expr = r.read()
        c.compile_form(expr, self.env, tail=False)

    def test_empty_toplevel_begin(self):
        self._test_toplevel_noerror('(begin)')

    def test_empty_expr_begin(self):
        self._test_expr_error('(begin)',
                              error='Empty "begin" expression')

    def test_empty_body_after_defines(self):
        self._test_toplevel_error('''
        (let ()
          (define x 100)
          (define y 200))
        ''', error='Empty body')

    def test_define_in_body(self):
        self._test_toplevel_error('''
        (let ((a 10))
          (define x 100)
          (begin
            (define y 200)
            (display "foo"))
          (define w 400)
          (list x y z w))
        ''', error='define is only allowed at')

    def test_duplicate_var_in_let(self):
        self._test_toplevel_error('''
        (let ((x 10) (x 20))
          x)
        ''', error='Duplicate variable')

    def test_duplicate_var_in_letrec(self):
        self._test_toplevel_error('''
        (letrec ((x 10) (x 20))
          x)
        ''', error='Duplicate variable')

    def test_duplicate_var_in_lambda(self):
        self._test_toplevel_error('''
        (lambda (x y x) x)
        ''', error='Duplicate variable')

    def test_duplicate_var_in_internal_define(self):
        self._test_toplevel_error('''
        (let ()
          (define x 10)
          (define x 20)
          x)
        ''', error='Duplicate definition')

    def test_redefine_local_var(self):
        self._test_toplevel_noerror('''
        (let ((x 10))
          (define x 20)
          x)
        ''')

    def test_lambda_with_empty_body(self):
        self._test_toplevel_error('''
        (lambda ()
          )
        ''', error='cannot be empty')

    def test_let_with_empty_body(self):
        self._test_toplevel_error('''
        (let ()
          )
        ''', error='cannot be empty')

    def test_letrec_with_empty_body(self):
        self._test_toplevel_error('''
        (letrec ()
          )
        ''', error='cannot be empty')

    def test_let_syntax_with_empty_body(self):
        self._test_toplevel_error('''
        (let-syntax ()
          )
        ''', error='cannot be empty')

    def test_letrec_syntax_with_empty_body(self):
        self._test_toplevel_error('''
        (letrec-syntax ()
          )
        ''', error='cannot be empty')

    def test_syntax_rules_just_ellipis(self):
        self._test_toplevel_error('''
        (letrec-syntax ((foo (syntax-rules ()
                               ((_) ...))))
          1)
        ''', error='single ellipsis')
