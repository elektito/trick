#!/usr/bin/env python3

from assemble import assemble
from compile import compile_form, compile_toplevel
from machinetypes import Bool
from read import print_sexpr, read
from secd import RunError, Secd


def main():
    with open('stdlib.lisp') as f:
        text = f.read()

    lib_asm = compile_toplevel(text)

    with open('test.lisp') as f:
        text = f.read()

    i = 0
    test_exprs = []
    while True:
        expr, i = read(text, i)
        if i >= len(text):
            break
        test_exprs.append(expr)

    for expr in test_exprs:
        expr_asm = compile_form(expr, [])
        asm = lib_asm + expr_asm
        code = assemble(asm)
        machine = Secd(code)
        try:
            machine.run()
        except RunError as e:
            print(f'error: {e}')
        else:
            result = machine.s[-1]
            if result:
                print('success')
            else:
                print('failure:')
                print('    ', end='')
                print_sexpr(expr)


if __name__ == '__main__':
    main()
