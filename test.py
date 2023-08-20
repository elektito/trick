#!/usr/bin/env python3

import argparse
import sys

from compile import ToplevelEnvironment, get_import_set
from exceptions import CompileError, RunError
from fasl import Fasl
from library import CoreLibrary, LibraryImportSet, LibraryName
from machinetypes import Symbol
from read import ReadError, Reader
from secd import Secd
from utils import compile_expr_to_fasl, ensure_stdlib


class TestFilter:
    def __init__(self, ranges=set()):
        self.ranges = ranges
        self.n_tests = None

    def set_tests_count(self, n: int):
        self.n_tests = n

    def match(self, i):
        for start, end in self.ranges:
            if start is None and end is None:
                return True
            elif start is None:
                if i <= end:
                    return True
            elif end is None:
                if i >= start:
                    return True
            else:
                if start <= i <= end:
                    return True

        return False


def test_filter(value):
    parts=value.split(',')
    ranges = []
    for part in parts:
        if part.startswith('-'):
            end = int(part[1:])
            ranges.append((None, end))
        elif part.endswith('-'):
            start = int(part[:-1])
            ranges.append((start, None))
        elif '-' in part:
            start = part[:part.index('-')]
            end = part[part.index('-')+1:]
            start = int(start)
            end = int(end)
            ranges.append((start, end))
        else:
            ranges.append((int(part), int(part)))
    return TestFilter(ranges)

def main():
    parser = argparse.ArgumentParser(description='Run test suite.')

    parser.add_argument(
        '--stop-on-failure', '-x', action='store_true', default=False,
        help='Stop on first failure.')

    parser.add_argument(
        '--verbose', '-v', action='store_true', default=False,
        help='Run tests in verbose mode.')

    parser.add_argument(
        '--filter', '-f', type=test_filter, default=None,
        help='Specify which tests to run by number. Ranges, and comma '
        'separated values are also accepted.')

    args = parser.parse_args()

    stdlib_fasl_filename = 'stdlib.fasl'
    ensure_stdlib(stdlib_fasl_filename)
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

    env = ToplevelEnvironment()
    env.add_import(LibraryImportSet(CoreLibrary()))
    env.add_import(
        get_import_set(
            LibraryName([Symbol('trick')]),
            libs))

    errors = []
    fails = []
    success = 0
    skips = 0
    for i, expr in enumerate(test_exprs, 1):
        if args.filter and not args.filter.match(i):
            skips += 1
            continue

        if args.verbose:
            print(f'[{i}] Running: {expr} ', end='', flush=True)

        try:
            expr_fasl = compile_expr_to_fasl(expr, libs, env=env)
        except CompileError as e:
            errors.append((i, expr, e))
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
            errors.append((i, expr, str(e)))
            if args.verbose:
                print('Error')
            else:
                print('E', end='', flush=True)
            if args.stop_on_failure:
                break
        else:
            if len(machine.s) != 1:
                if len(machine.s) == 0:
                    errors.append((i, expr, 'Expression did not leave anything on the stack'))
                else:
                    errors.append((i, expr, f'Expression left more than one value on the stack ({machine.s})'))
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
                        (i, expr,
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
                        fails.append((i, expr))
                        if args.stop_on_failure:
                            break

    print()
    if fails:
        print('Failed test case(s):')
        for i, expr in fails:
            print(f'   [{i}] ', end='')
            print(expr)

    if errors:
        print('Error(s):')
        for i, expr, err in errors:
            print(f'    [{i}] {err}: ', end='')
            print(expr)

    if fails or errors or skips:
        print()
        if skips:
            print(f'Skipped: {skips}  Failed: {len(fails)}  Error: {len(errors)}  Success: {success}')
        else:
            print(f'Failed: {len(fails)}  Error: {len(errors)}  Success: {success}')
    else:
        print(f'{len(test_exprs)} test(s) finished successfully.')

if __name__ == '__main__':
    main()
