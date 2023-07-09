#!/usr/bin/env python3

import sys
import argparse
from read import read, ParseError
from machinetypes import String, Symbol
from symtab import Symtab


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
            raise AssembleError(f'Instruction not a symbol: {instr}')

        instr = instr.name
        single_byte_instrs = {
            'nil': 0x01,
            'true': 0x02,
            'false': 0x03,
            'cons': 0x04,
            'car': 0x05,
            'cdr': 0x06,
            'join': 0x07,
            'ap': 0x08,
            'ret': 0x09,
            'tap': 0x0a,
            'dum': 0x0b,
            'rap': 0x0c,
            'print': 0x0d,
            'printc': 0x0e,
            'halt': 0x0f,
            'iadd': 0x10,
            'isub': 0x11,
            'imul': 0x12,
            'idiv': 0x13,
            'shr': 0x14,
            'shl': 0x15,
            'asr': 0x16,
            'bnot': 0x17,
            'band': 0x18,
            'bor': 0x19,
            'bxor': 0x1a,
            'lt': 0x1b,
            'le': 0x1c,
            'eq': 0x1d,
            'drop': 0x1e,
            'dup': 0x1f,
            'xp': 0x20,
            'type': 0x21,
            'error': 0x22,
            'gensym': 0x23,
        }
        opcode = single_byte_instrs.get(instr)
        if opcode is not None:
            code += bytes([opcode])
        elif instr == 'ldc':
            value = expr[i]
            i += 1
            if not isinstance(value, int):
                raise AssembleError(f'Invalid argument for ldc: {value}')
            code += bytes([0x40])
            code += value.to_bytes(length=4, byteorder='little', signed=True)
        elif instr == 'ld':
            if not isinstance(expr[i], list) or \
               len(expr[i]) != 2 or \
               not isinstance(expr[i][0], int) or \
               not isinstance(expr[i][1], int):
                raise AssembleError(f'Invalid argument for ld: {expr[i]}')
            frame, index = expr[i]
            i += 1
            code += bytes([0x41])
            code += frame.to_bytes(length=2, byteorder='little', signed=False)
            code += index.to_bytes(length=2, byteorder='little', signed=False)
        elif instr == 'sel':
            if not isinstance(expr[i], list) or not isinstance(expr[i + 1], list):
                raise AssembleError(f'Invalid argument for sel: {expr[j]}')

            code += bytes([0x42])

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
            code += bytes([0x43])
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
            code += bytes([0x44])
            code += frame.to_bytes(length=2, byteorder='little', signed=False)
            code += index.to_bytes(length=2, byteorder='little', signed=False)
        elif instr == 'ldfx':
            if not isinstance(expr[i], list):
                raise AssembleError(f'Invalid argument for ldfx: {expr[i]}')
            args = expr[i]
            i += 1

            if len(args) != 2 or \
               not isinstance(args[0], int) or \
               not isinstance(args[1], list):
                raise AssembleError(f'Invalid argument for ldfx: {expr[i]}')

            nargs, body = args
            if nargs < 0 or nargs > 255:
                raise AssembleError(f'Invalid nargs value for ldfx: {nargs}')

            code += bytes([0x45])
            code += bytes([nargs])
            body_code = assemble(body, start_offset + len(code) + 4)
            code += len(body_code).to_bytes(length=4, byteorder='little', signed=False)
            code += body_code
        elif instr == 'ldstr':
            value = expr[i]
            i += 1
            if not isinstance(value, int):
                raise AssembleError(f'Invalid argument for ldstr: {value}')
            code += bytes([0x46])
            code += value.to_bytes(length=4, byteorder='little', signed=True)
        elif instr == 'strtab':
            strnums = expr[i]
            i += 1
            if not isinstance(strnums, list):
                raise AssembleError(f'Invalid argument type for strtab: {strnums}')
            if not all(isinstance(s, String) for s in strnums):
                raise AssembleError(f'Non-string value passed to strtab: {strnums}')
            code += bytes([0x47])
            code += len(strnums).to_bytes(length=4, byteorder='little', signed=False)
            for s in strnums:
                code += len(s).to_bytes(length=4, byteorder='little', signed=False)
                code += s.encode('utf-8')
        elif instr == 'ldsym':
            value = expr[i]
            i += 1
            if not isinstance(value, int):
                raise AssembleError(f'Invalid argument for ldstr: {value}')
            code += bytes([0x48])
            code += value.to_bytes(length=4, byteorder='little', signed=True)
        elif instr == 'symtab':
            strnums = expr[i]
            i += 1
            if not isinstance(strnums, list):
                raise AssembleError(f'Invalid argument type for symtab: {strnums}')
            if not all(isinstance(s, int) for s in strnums):
                raise AssembleError(f'Non-numeric value passed to symtab: {strnums}')
            code += bytes([0x49])
            code += len(strnums).to_bytes(length=4, byteorder='little', signed=False)
            for n in strnums:
                code += n.to_bytes(length=4, byteorder='little', signed=False)
        elif instr == 'set':
            symnum = expr[i]
            i += 1
            if not isinstance(symnum, int):
                raise AssembleError(f'Invalid argument type for set: {symnum}')
            code += bytes([0x4a])
            code += symnum.to_bytes(length=4, byteorder='little', signed=False)
        elif instr == 'get':
            symnum = expr[i]
            i += 1
            if not isinstance(symnum, int):
                raise AssembleError(f'Invalid argument type for get: {symnum}')
            code += bytes([0x4b])
            code += symnum.to_bytes(length=4, byteorder='little', signed=False)
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
        expr, _ = read(text, symtab=Symtab())
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
