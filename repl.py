#!/usr/bin/env python3

import argparse
import readline
from assemble import Assembler
from compile import Compiler, CompileError
from read import ParseError, read
from secd import RunError, Secd, UserError
from utils import format_user_error


def configure_argparse(parser: argparse.ArgumentParser):
    parser.description = 'Run a Trick REPL'
    parser.set_defaults(func=main)


def main(args):
    with open('stdlib.scm') as f:
        text = f.read()

    compiler = Compiler()
    lib_asm = compiler.compile_toplevel(text)

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
            expr_asm = compiler.compile_form(expr, [], 0)
        except CompileError as e:
            print(f'Compile error: {e}')
            continue

        assembler = Assembler()
        asm = lib_asm + expr_asm
        code = assembler.assemble(asm)
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
