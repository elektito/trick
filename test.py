#!/usr/bin/env python3

import argparse
from assemble import assemble
from compile import CompileError, compile_form, compile_toplevel
from machinetypes import Symbol
from read import read
from secd import RunError, Secd, UserError
from utils import assoc


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
    success = 0
    for expr in test_exprs:
        if args.verbose:
            print('Running: ', end='')
            print(expr, end='')
            print(' ', end='', flush=True)
        try:
            expr_asm = compile_form(expr, [])
        except CompileError as e:
            errors.append((expr, e))
            if args.verbose:
                print('Error')
            else:
                print('E', end='', flush=True)
            if args.stop_on_failure:
                break
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
                print('Error')
            else:
                print('E', end='', flush=True)
            if args.stop_on_failure:
                break
        except RunError as e:
            errors.append((expr, str(e)))
            if args.verbose:
                print('Error')
            else:
                print('E', end='', flush=True)
            if args.stop_on_failure:
                break
        else:
            if len(machine.s) != 1:
                if len(machine.s) == 0:
                    errors.append((expr, 'Expression did not return anything'))
                else:
                    errors.append((expr, f'Expression returned more than one value (stack={machine.s})'))
                if args.verbose:
                    print('Error')
                else:
                    print('E', end='', flush=True)
                if args.stop_on_failure:
                    break
            else:
                result = machine.s[-1]
                success += 1
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
        print('Failed test case(s):')
        for expr in fails:
            print('    ', end='')
            print(expr)

    if errors:
        print('Error(s):')
        for expr, err in errors:
            print(f'    {err}: ', end='')
            print(expr)

    if fails or errors:
        print()
        print(f'Failed: {len(fails)}  Error: {len(errors)}  Success: {success}')
    else:
        print(f'{len(test_exprs)} test(s) finished successfully.')

if __name__ == '__main__':
    main()
