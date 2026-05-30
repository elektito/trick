import unittest
from trick.compile import Compiler
from trick.machinetypes import Symbol
from trick.optimize import Optimizer
from trick.utils import init_stdlib

def S(s):
    return Symbol(s)

class TestOptimization(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        init_stdlib()

    def test_void_drop_elimination(self):
        # (begin #$void 1) produces a void followed by drop without
        # optimization. With optimization both should be eliminated.
        code = "(import (scheme base)) (begin #$void 1)"
        compiler = Compiler()
        program = compiler.compile_program(code)

        names = [x.name for x in program.code if isinstance(x, Symbol) and not x.name.startswith(':')]
        self.assertNotIn('void', names)
        self.assertNotIn('drop', names)

    def test_nested_optimization(self):
        # Optimizer must recurse into lambda bodies.
        code = "(import (scheme base)) (define foo (lambda () (begin #$void 1)))"
        compiler = Compiler()
        program = compiler.compile_program(code)

        asm = program.code.to_list_recursive()
        ldf_body = None
        for i in range(len(asm)):
            if isinstance(asm[i], Symbol) and asm[i].name == 'ldf':
                ldf_body = asm[i+2]
                break

        self.assertIsNotNone(ldf_body)
        names = [x.name for x in ldf_body if isinstance(x, Symbol) and not x.name.startswith(':')]
        self.assertNotIn('void', names)
        self.assertNotIn('drop', names)

    def test_disabled_optimization(self):
        # With opt_level=0, pure pushes followed by drop must not be
        # eliminated.
        code = "(import (scheme base)) #t #f 1"
        compiler = Compiler(opt_level=0)
        program = compiler.compile_program(code)

        def flatten_names(ls):
            for item in ls:
                if isinstance(item, list):
                    yield from flatten_names(item)
                elif isinstance(item, Symbol):
                    yield item.name

        names = list(flatten_names(program.code.to_list_recursive()))
        self.assertTrue(any(
            names[i] == 'true' and names[i+1] == 'drop'
            for i in range(len(names) - 1)
        ))

class TestOptimizerTruncation(unittest.TestCase):
    def test_truncated_stream_raises(self):
        # ldc requires 1 argument. A stream ending immediately after it
        # must raise rather than silently emitting incomplete code.
        opt = Optimizer()
        with self.assertRaises(ValueError):
            opt.optimize([S('ldc')])


if __name__ == '__main__':
    unittest.main()
