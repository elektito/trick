import os
import atexit
import argparse
import readline
from compile import CompileError, CoreImportSet, LibraryImportSet, ToplevelEnvironment, primcalls
from fasl import Fasl
from library import LibraryName
from machinetypes import Symbol, Void
from read import ReadError, read_expr
from runtime import TrickExitException
from secd import RunError, Secd
from utils import compile_expr_to_fasl, ensure_stdlib


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
    def __init__(self, env):
        self.env = env

    def __call__(self, text: str, state: int):
        if state == 0:
            self.syms = [
                s.name
                for s in self.env.get_all_names()
                if s.name.startswith(text)
            ]

        try:
            return self.syms[state]
        except IndexError:
            return None


def main(args):
    stdlib_fasl_filename = 'stdlib.fasl'
    ensure_stdlib(stdlib_fasl_filename)
    with open(stdlib_fasl_filename, 'rb') as f:
        stdlib_fasl = Fasl.load(f, stdlib_fasl_filename)

    libs = [stdlib_fasl]

    env = ToplevelEnvironment()
    env.add_import(CoreImportSet())
    env.add_import(
        LibraryImportSet.get_import_set(
            LibraryName([Symbol('trick')]),
            libs))

    readline.set_auto_history(True)
    read_history()
    atexit.register(write_history)

    readline.parse_and_bind("tab: complete")
    readline.set_completer_delims('()[] \'"|')
    readline.set_completer(Completer(env))

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
            expr_fasl = compile_expr_to_fasl(expr, libs, env)
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
