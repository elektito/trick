import os
import atexit
import argparse
import readline
from assemble import Assembler
from compile import CompileError
from fasl import Fasl
from machinetypes import Void
from read import ReadError, read_expr
from runtime import TrickExitException
from secd import RunError, Secd
from utils import compile_expr_to_fasl, ensure_fasl


HISTORY_FILE = os.path.expanduser('~/.trick-repl-history')


def configure_argparse(parser: argparse.ArgumentParser):
    parser.description = 'Run a Trick REPL'
    parser.set_defaults(func=main)


def read_history():
    try:
        readline.read_history_file(HISTORY_FILE)
    except FileNotFoundError:
        pass


def write_history():
    readline.write_history_file(HISTORY_FILE)



class Completer:
    def __init__(self, libs):
        self.libs = libs

    def __call__(self, text: str, state: int):
        if state == 0:
            syms = set()
            for lib in self.libs:
                syms |= set(s for s in lib.defines.keys() if s.name.startswith(text))
            self.syms = list(sorted(syms, key=lambda s: s.name))

        try:
            return self.syms[state].name
        except IndexError:
            return None


def main(args):
    stdlib_src_filename = 'stdlib.scm'
    stdlib_fasl_filename = 'stdlib.fasl'
    ensure_fasl(stdlib_src_filename)
    with open(stdlib_fasl_filename, 'rb') as f:
        stdlib_fasl = Fasl.load(f, stdlib_fasl_filename)

    libs = [stdlib_fasl]

    readline.set_auto_history(True)
    read_history()
    atexit.register(write_history)

    readline.parse_and_bind("tab: complete")
    readline.set_completer_delims('()[] \'"|')
    readline.set_completer(Completer(libs))

    machine = Secd()
    machine.load_fasls(libs)

    while True:
        try:
            text = input('> ')
        except (EOFError, KeyboardInterrupt):
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

        try:
            machine.execute_fasl(expr_fasl)
        except TrickExitException as e:
            print('Expression exited with code:', e.exit_code)
        except RunError as e:
            print(e)
            machine.print_stack_trace()
        except KeyboardInterrupt:
            print('Interrupted')
            machine.print_stack_trace()
        else:
            result = machine.s.pop_multiple()
            result = result.as_list()
            if result != [Void()]:
                for r in result:
                    print(r)

        # if the expression was a definition, add the fasl to the our list of
        # libraries, so it's available to future expressions.
        if len(expr_fasl.defines) > 0:
            libs.append(expr_fasl)
