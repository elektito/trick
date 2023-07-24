#!/usr/bin/env python3

import argparse
import readline
from assemble import Assembler
from compile import Compiler, CompileError
from fasl import Fasl
from read import ReadError, read_expr
from secd import RunError, Secd, UserError
from utils import compile_expr_to_fasl, ensure_fasl, format_user_error


def configure_argparse(parser: argparse.ArgumentParser):
    parser.description = 'Run a Trick REPL'
    parser.set_defaults(func=main)


def main(args):
    stdlib_src_filename = 'stdlib.scm'
    stdlib_fasl_filename = 'stdlib.fasl'
    ensure_fasl(stdlib_src_filename)
    with open(stdlib_fasl_filename, 'rb') as f:
        stdlib_fasl = Fasl.load(f, stdlib_fasl_filename)

    libs = [stdlib_fasl]

    while True:
        try:
            text = input('> ')
        except EOFError:
            print()
            break

        if text.strip() == '':
            continue

        try:
            expr = read_expr(text)
        except ReadError as e:
            print(f'Read error: {e}')
            continue

        try:
            expr_fasl = compile_expr_to_fasl(expr, libs)
        except CompileError as e:
            print(f'Compile error: {e}')
            continue

        machine = Secd(expr_fasl, libs)
        try:
            machine.run()
        except UserError:
            err = machine.s.top()
            msg = format_user_error(err)
            print(msg)
        except RunError as e:
            print(e)
        else:
            result = machine.s.pop_multiple()
            for r in result.as_list():
                print(r)

        # if the expression was a definition, add the fasl to the our list of
        # libraries, so it's available to future expressions.
        if len(expr_fasl.defines) > 0:
            libs.append(expr_fasl)
