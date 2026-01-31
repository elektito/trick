import unittest
from trick.compile import Compiler
from trick.machinetypes import Symbol
from trick.utils import init_stdlib

def S(s):
    return Symbol(s)

class TestOptimization(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        init_stdlib()

    def test_void_drop_elimination(self):
        # A begin block with a non-final void-returning expression should 
        # normally produce (void) (drop)
        code = "(import (scheme base)) (begin #$void 1)"
        compiler = Compiler()
        program = compiler.compile_program(code)
        
        asm = program.code
        
        # 'void' and 'drop' should be gone.
        names = [x.name for x in asm if isinstance(x, Symbol) and not x.name.startswith(':')]
        # After (import (scheme base)), there might be some setup code, 
        # but let's check for the void-drop sequence specifically.
        
        # Actually, let's just check if 'void' exists.
        self.assertNotIn('void', names)

    def test_nested_optimization(self):
        # Test that optimization happens inside lambda
        code = "(import (scheme base)) (define foo (lambda () (begin #$void 1)))"
        compiler = Compiler()
        program = compiler.compile_program(code)
        
        # Look for the ldf instruction in the code
        # Code should contain [..., ldf, 0, [body], set, foo, void, drop]
        # (void and drop because it's a top-level define)
        
        asm = program.code.to_list_recursive()
        ldf_body = None
        for i in range(len(asm)):
            if isinstance(asm[i], Symbol) and asm[i].name == 'ldf':
                ldf_body = asm[i+2]
                break
        
        self.assertIsNotNone(ldf_body)
        names = [x.name for x in ldf_body if isinstance(x, Symbol) and not x.name.startswith(':')]
        
        self.assertNotIn('void', names)
        self.assertIn('ldc', names)
        self.assertIn('ret', names)

    def test_disabled_optimization(self):
        # With opt_level=0, pure pushes followed by drop should remain
        code = "(import (scheme base)) #t #f 1"
        compiler = Compiler(opt_level=0)
        program = compiler.compile_program(code)
        
        asm = program.code.to_list_recursive()
        
        def flatten(ls):
            res = []
            for i in ls:
                if isinstance(i, list):
                    res.extend(flatten(i))
                elif isinstance(i, Symbol):
                    res.append(i.name)
                elif isinstance(i, int):
                    res.append(str(i))
                else:
                    res.append(str(i))
            return res

        flat_asm = flatten(asm)
        
        # Check if 'true' followed by 'drop' exists
        found_true_drop = False
        for i in range(len(flat_asm) - 1):
            if flat_asm[i] == 'true' and flat_asm[i+1] == 'drop':
                found_true_drop = True
                break
        
        self.assertTrue(found_true_drop)

if __name__ == '__main__':
    unittest.main()
