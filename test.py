#!/usr/bin/env python3

import argparse
from assemble import assemble
from compile import CompileError, assoc, compile_form, compile_toplevel
from machinetypes import Symbol
from read import print_sexpr, read
from secd import RunError, Secd, UserError


def main():
    parser = argparse.ArgumentParser(description='Run test suite.')

    parser.add_argument(
        '--stop-on-failure', '-x', action='store_true', default=False,
        help='Stop on first failure.')

    parser.add_argument(
        '--verbose', '-v', action='store_true', default=False,
        help='Run tests in verbose mode.')

    args = parser.parse_args()

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

    errors = []
    fails = []
    for expr in test_exprs:
        if args.verbose:
            print('Running: ', end='')
            print_sexpr(expr, end='')
            print(' ', end='', flush=True)
        try:
            expr_asm = compile_form(expr, [])
        except CompileError as e:
            errors.append((expr, e))
            if args.stop_on_failure:
                break
            if args.verbose:
                print()
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
            errors.append((expr, msg))
            if args.verbose:
                print('Success')
            else:
                print('E', end='', flush=True)
            if args.stop_on_failure:
                break
        except RunError as e:
            errors.append((expr, str(e)))
            print('E', end='', flush=True)
            if args.stop_on_failure:
                break
        else:
            result = machine.s[-1]
            if result:
                if args.verbose:
                    print('Success')
                else:
                    print('.', end='', flush=True)
            else:
                if args.verbose:
                    print('Failed')
                else:
                    print('F', end='', flush=True)
                fails.append(expr)

    print()
    if fails:
        print()
        print('Failed test case(s):')
        for expr in fails:
            print('    ', end='')
            print_sexpr(expr)

    if errors:
        print()
        print('Error(s):')
        for expr, exception in errors:
            if isinstance(exception, UserError):
                msg = xx
            else:
                msg = str(exception)
            print(f'    {msg}: ', end='')
            print_sexpr(expr)

    if fails or errors:
        print()
        success = len(test_exprs) - len(fails) - len(errors)
        print(f'Failed: {len(fails)}  Error: {len(errors)}  Success: {success}')
    else:
        print(f'{len(test_exprs)} test(s) finished successfully.')

if __name__ == '__main__':
    main()