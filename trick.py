#!/usr/bin/env python3

import argparse
import sys
import os

import compile
import assemble
import fasl
from machinetypes import List
from read import ReadError, read_expr
import secd
import repl
from utils import compile_expr_to_fasl, compile_src_file_to_fasl, load_fasl_files
from version import __version__


def main():
    parser = argparse.ArgumentParser(description='Trick Scheme System')

    parser.add_argument(
        '--version', '-V', action='store_true',
        help='Show version and exit.')

    parser.add_argument(
        '--compile', '-c', metavar='SRC_FILE', default=None,
        help='Compile the given file into a FASL.')

    parser.add_argument(
        '--compile-expr', '-C', metavar='EXPR',
        help='Compile the given expression into assembly.')

    parser.add_argument(
        '--lib', '-l', metavar='FASL', action='append', default=[],
        help='Load the given FASL as a library when compiling.')

    parser.add_argument(
        '--include-path', '-I', action='append', default=[],
        help='Add a given path to the include search path. Can be '
        'specified multiple times.')

    parser.add_argument(
        '--macro-expand', '-m', metavar='EXPR', dest='macro_expr',
        help='Macro-expand the given expression (only first term is '
        'expanded).')

    parser.add_argument(
        '--macro-expand-full', '-M', metavar='EXPR', dest='macro_expr_full',
        help='Macro-expand the given expression (full).')

    parser.add_argument(
        '--eval', '-e', metavar='EXPR', dest='eval_expr',
        help='Compile and run the given expression, and print the result.')

    parser.add_argument(
        '--output', '-o', metavar='FILENAME', default=None,
        help='The name of the output FASL file. If not specified, '
        'defaults to the name of the input file with the extension '
        'changed to .fasl')

    parser.add_argument(
        '--dbg-info', '-g', action='store_true', default=False,
        help='Enable adding debug info to FASL.')

    subparsers = parser.add_subparsers(help='Trick commands')

    compile_parser = subparsers.add_parser('compile')
    compile.configure_argparse(compile_parser)

    assemble_parser = subparsers.add_parser('assemble')
    assemble.configure_argparse(assemble_parser)

    run_parser = subparsers.add_parser('run')
    secd.configure_argparse(run_parser)

    repl_parser = subparsers.add_parser('repl')
    repl.configure_argparse(repl_parser)

    fasl_parser = subparsers.add_parser('fasl')
    fasl.configure_argparse(fasl_parser)

    args = parser.parse_args()

    if args.version:
        print(f'Trick Version: v{__version__}')
        sys.exit(0)

    if args.compile:
        if not args.output:
            name, _ = os.path.splitext(args.compile)
            args.output = name + '.fasl'
        try:
            compile_src_file_to_fasl(
                args.compile, args.output, args.lib,
                include_paths=args.include_path,
                dbg_info=args.dbg_info)
        except compile.CompileError as e:
            print('Compile error:', e)
            e.print_snippet()
            sys.exit(1)
    elif args.compile_expr:
        lib_fasls = load_fasl_files(args.lib)
        compiler = compile.Compiler(lib_fasls, debug_info=args.dbg_info)
        try:
            asm = compiler.compile_toplevel(args.compile_expr)
        except compile.CompileError as e:
            print('Compile error:', e)
            sys.exit(1)
        asm = List.from_list_recursive(asm)
        print(asm)
    elif args.macro_expr:
        try:
            form = read_expr(args.macro_expr)
        except ReadError as e:
            print(f'Read error: {e}')
            sys.exit(1)

        try:
            lib_fasls = load_fasl_files(args.lib)
        except FileNotFoundError as e:
            print(e)
            sys.exit(1)
        compiler = compile.Compiler(lib_fasls, debug_info=args.dbg_info)
        try:
            expanded = compiler.macro_expand(form, compile.Environment())
        except compile.CompileError as e:
            print('Compile error:', e)
            sys.exit(1)

        print(expanded)
    elif args.macro_expr_full:
        try:
            form = read_expr(args.macro_expr_full)
        except ReadError as e:
            print(f'Read error: {e}')
            sys.exit(1)

        lib_fasls = load_fasl_files(args.lib)
        compiler = compile.Compiler(lib_fasls, debug_info=args.dbg_info)
        try:
            expanded = compiler.macro_expand_full(form, compile.Environment())
        except compile.CompileError as e:
            print('Compile error:', e)
            sys.exit(1)

        print(expanded)
    elif args.eval_expr:
        lib_fasls = load_fasl_files(args.lib)
        try:
            expr = read_expr(args.eval_expr)
        except ReadError as e:
            print(f'Read error: {e}')
            sys.exit(1)

        try:
            expr_fasl = compile_expr_to_fasl(expr, lib_fasls)
        except compile.CompileError as e:
            print(f'Compile error: {e}')
            sys.exit(1)

        machine = secd.Secd(lib_fasls)
        try:
            machine.execute_fasl(expr_fasl)
        except secd.RunError as e:
            print(e)
        else:
            result = machine.s.pop_multiple()
            for r in result.as_list():
                print(r)
    else:
        if not hasattr(args, 'func'):
            # no sub-command specified. default to repl.
            args = parser.parse_args(['repl'])

        args.func(args)


if __name__ == '__main__':
    main()
