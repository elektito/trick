#!/usr/bin/env python3

import argparse
import sys

import compile
import assemble
import secd
import repl


VERSION = '0.1.0'


def main():
    parser = argparse.ArgumentParser(description='Trick Scheme System')

    parser.add_argument(
        '--version', '-V', action='store_true',
        help='Show version and exit.')

    subparsers = parser.add_subparsers(help='Trick commands')

    compile_parser = subparsers.add_parser('compile')
    compile.configure_argparse(compile_parser)

    assemble_parser = subparsers.add_parser('assemble')
    assemble.configure_argparse(assemble_parser)

    run_parser = subparsers.add_parser('run')
    secd.configure_argparse(run_parser)

    repl_parser = subparsers.add_parser('repl')
    repl.configure_argparse(repl_parser)

    args = parser.parse_args()

    if args.version:
        print(f'Trick Version: v{VERSION}')
        sys.exit(0)

    if not hasattr(args, 'func'):
        # no sub-command specified. default to repl.
        args = parser.parse_args(['repl'])

    args.func(args)


if __name__ == '__main__':
    main()
