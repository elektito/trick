#!/usr/bin/env python3

import argparse
import readline
from assemble import assemble
from compile import CompileError, compile_form, compile_toplevel
from machinetypes import Symbol
from read import ParseError, read
from secd import RunError, Secd, UserError
from utils import format_user_error


def configure_argparse(parser: argparse.ArgumentParser):
    parser.description = 'Run a Trick REPL'
    parser.set_defaults(func=main)


def main(args):
    with open('stdlib.scm') as f:
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
            err = machine.s.top()
            msg = format_user_error(err)
            print(msg)
        except RunError as e:
            print(e)
        else:
            result = machine.s.pop_multiple()
            for r in result.as_list():
                print(r)
