#!/usr/bin/env python3

import sys
import argparse
from sexpr import read, ParseError, Symbol


class AssembleError(Exception):
    pass


def assemble(expr, start_offset: int = 0) -> bytes:
    if not isinstance(expr, list):
        raise AssembleError('Input not a list')

    i = 0
    code = b''
    while i < len(expr):
        instr = expr[i]
        i += 1

        if not isinstance(instr, Symbol):
            raise AssembleError(f'Invalid instruction: {instr}')

        instr = instr.name
        single_byte_instrs = {
            'nil': 0x01,
            'cons': 0x02,
            'join': 0x03,
            'ap': 0x04,
            'ret': 0x05,
            'printn': 0x06,
            'printc': 0x07,
            'halt': 0x08,
            'add': 0x09,
            'sub': 0x0a,
            'lt': 0x0b,
            'dum': 0x0c,
            'rap': 0x0d,
            'tap': 0x0e,
            'drop': 0x0f,
            'xp': 0x10,
            'dup': 0x11,
            'true': 0x12,
            'false': 0x13,
        }
        opcode = single_byte_instrs.get(instr)
        if opcode is not None:
            code += bytes([opcode])
        elif instr == 'ldc':
            value = expr[i]
            i += 1
            if not isinstance(value, int):
                raise AssembleError(f'Invalid argument for ldc: {value}')
            code += bytes([0x20])
            code += value.to_bytes(length=4, byteorder='little', signed=True)
        elif instr == 'ld':
            if not isinstance(expr[i], list) or \
               len(expr[i]) != 2 or \
               not isinstance(expr[i][0], int) or \
               not isinstance(expr[i][1], int):
                raise AssembleError(f'Invalid argument for ld: {expr[i]}')
            frame, index = expr[i]
            i += 1
            code += bytes([0x21])
            code += frame.to_bytes(length=2, byteorder='little', signed=False)
            code += index.to_bytes(length=2, byteorder='little', signed=False)
        elif instr == 'sel':
            if not isinstance(expr[i], list) or not isinstance(expr[i + 1], list):
                raise AssembleError(f'Invalid argument for sel: {expr[j]}')

            code += bytes([0x22])

            true_body = assemble(expr[i], start_offset + 8)
            false_body = assemble(expr[i + 1], start_offset + len(code) + 8 + len(true_body))

            code += len(true_body).to_bytes(length=4, byteorder='little', signed=False)
            code += len(false_body).to_bytes(length=4, byteorder='little', signed=False)
            code += true_body
            code += false_body

            i += 2
        elif instr == 'ldf':
            if not isinstance(expr[i], list):
                raise AssembleError(f'Invalid argument for ldf: {expr[i]}')
            body = expr[i]
            i += 1
            code += bytes([0x23])
            body_code = assemble(body, start_offset + len(code) + 4)
            code += len(body_code).to_bytes(length=4, byteorder='little', signed=False)
            code += body_code
        elif instr == 'st':
            if not isinstance(expr[i], list) or \
               len(expr[i]) != 2 or \
               not isinstance(expr[i][0], int) or \
               not isinstance(expr[i][1], int):
                raise AssembleError(f'Invalid argument for st: {expr[i]}')
            frame, index = expr[i]
            i += 1
            code += bytes([0x24])
            code += frame.to_bytes(length=2, byteorder='little', signed=False)
            code += index.to_bytes(length=2, byteorder='little', signed=False)
        else:
            raise AssembleError(f'Unknown instruction: {instr}')

    return code


def main():
    parser = argparse.ArgumentParser(
        description='Assemble SECD instructions written in sexpr form '
        'into binary machine code.')

    parser.add_argument(
        'input', default='-', nargs='?',
        help='Input file. Stdin is used if not specified or a dash (-) '
        'is passed instead. Defaults to reading from stdin.')

    args = parser.parse_args()

    if args.input == '-':
        text = sys.stdin.read()
    else:
        with open(args.input) as f:
            text = f.read()

    try:
        expr, _ = read(text)
    except ParseError as e:
        print(f'Parse error: {e}', file=sys.stderr)
        sys.exit(1)

    try:
        code = assemble(expr, 0)
    except AssembleError as e:
        print(f'Assemble error: {e}', file=sys.stderr)
        sys.exit(1)

    sys.stdout.buffer.write(code)


if __name__ == '__main__':
    main()
