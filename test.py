#!/usr/bin/env python3

import argparse
import sys
from compile import CompileError
from fasl import Fasl
from read import ReadError, Reader
from secd import RunError, Secd
from utils import compile_expr_to_fasl, ensure_fasl


def main():
    parser = argparse.ArgumentParser(description='Run test suite.')

    parser.add_argument(
        '--stop-on-failure', '-x', action='store_true', default=False,
        help='Stop on first failure.')

    parser.add_argument(
        '--verbose', '-v', action='store_true', default=False,
        help='Run tests in verbose mode.')

    args = parser.parse_args()

    stdlib_src_filename = 'stdlib.scm'
    stdlib_fasl_filename = 'stdlib.fasl'
    ensure_fasl(stdlib_src_filename)
    with open(stdlib_fasl_filename, 'rb') as f:
        stdlib_fasl = Fasl.load(f, stdlib_fasl_filename)

    libs = [stdlib_fasl]

    f = open('test.scm')
    reader = Reader(f)

    i = 0
    test_exprs = []
    while True:
        try:
            expr = reader.read()
        except ReadError as e:
            print(f'Read error: {e}')
            sys.exit(1)
        if expr is None:
            break
        test_exprs.append(expr)

    machine = Secd(libs)

    errors = []
    fails = []
    success = 0
    for i, expr in enumerate(test_exprs):
        if args.verbose:
            print(f'[{i+1}] Running: {expr} ', end='', flush=True)

        try:
            expr_fasl = compile_expr_to_fasl(expr, libs)
        except CompileError as e:
            errors.append((expr, e))
            if args.verbose:
                print('Error')
            else:
                print('E', end='', flush=True)
            if args.stop_on_failure:
                break
            continue

        try:
            machine.execute_fasl(expr_fasl)
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
                    errors.append((expr, 'Expression did not leave anything on the stack'))
                else:
                    errors.append((expr, f'Expression left more than one value on the stack ({machine.s})'))
                if args.verbose:
                    print('Error')
                else:
                    print('E', end='', flush=True)
                if args.stop_on_failure:
                    break
            else:
                result = machine.s.pop_multiple()
                if len(result) != 1:
                    errors.append(
                        (expr,
                         f'Expression returned {len(result)} values '
                         f'instead of a single boolean'))
                    if args.verbose:
                        print('Error')
                    else:
                        print('E', end='', flush=True)
                    if args.stop_on_failure:
                        break
                else:
                    result = result[0]
                    if result:
                        success += 1
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
                        if args.stop_on_failure:
                            break

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
