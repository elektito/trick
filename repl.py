#!/usr/bin/env python3

import argparse
from assemble import assemble
from compile import CompileError, assoc, compile_form, compile_toplevel
from machinetypes import Symbol
from read import ParseError, print_sexpr, read
from secd import RunError, Secd, UserError


def main():
    parser = argparse.ArgumentParser(description='Run test suite.')
    args = parser.parse_args()

    with open('stdlib.lisp') as f:
        text = f.read()

    lib_asm = compile_toplevel(text)

    while True:
        try:
            text = input('> ')
        except EOFError:
            print()
            break

        if text.strip() == '':
            continue

        try:
            expr, _ = read(text, 0)
        except ParseError as e:
            print(f'Parse error: {e}')
            continue

        try:
            expr_asm = compile_form(expr, [])
        except CompileError as e:
            print(f'Compile error: {e}')
            continue

        asm = lib_asm + expr_asm
        code = assemble(asm)
        machine = Secd(code)
        try:
            machine.run()
        except UserError:
            err = machine.s[-1]
            err_type = assoc(Symbol(':type'), err)
            msg = f'User error of type {err_type} during macro expansion'
            err_msg = assoc(Symbol(':msg'), err)
            if err_msg is not None:
                msg += f': {err_msg}'
            print(msg)
        except RunError as e:
            print(e)
        else:
            result = machine.s[-1]
            print_sexpr(result)


if __name__ == '__main__':
    main()
