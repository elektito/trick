#!/usr/bin/env python3

import sys
import argparse
from read import read, ParseError
from machinetypes import Pair, String, Symbol


class AssembleError(Exception):
    pass


def _assemble(expr, start_offset: int, strings, symbols) -> bytes:
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
            'irem': 0x14,
            'shr': 0x15,
            'shl': 0x16,
            'asr': 0x17,
            'bnot': 0x18,
            'band': 0x19,
            'bor': 0x1a,
            'bxor': 0x1b,
            'ilt': 0x1c,
            'ile': 0x1d,
            'eq': 0x1e,
            'drop': 0x1f,
            'dup': 0x20,
            'xp': 0x21,
            'type': 0x22,
            'error': 0x23,
            'gensym': 0x24,
            'ccc': 0x25,
            'i2ch': 0x26,
            'ch2i': 0x27,
            'ugcat': 0x28,
            'chup': 0x29,
            'chdn': 0x2a,
            'chfd': 0x2b,
            'chdv': 0x2c,
            'mkstr': 0x2d,
            'strref': 0x2e,
            'strset': 0x2f,
            'strlen': 0x30,
            'setcar': 0x31,
            'setcdr': 0x32,
            'm2l': 0x33,
            'l2m': 0x34,
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
            if not isinstance(expr[i], list) :
                raise AssembleError(f'Invalid first argument for sel: {expr[i]}')
            if not isinstance(expr[i + 1], list):
                raise AssembleError(f'Invalid second argument for sel: {expr[i+1]}')

            code += bytes([0x42])

            true_body = _assemble(expr[i], start_offset + 8, strings, symbols)
            false_body = _assemble(expr[i + 1], start_offset + len(code) + 8 + len(true_body), strings, symbols)

            code += len(true_body).to_bytes(length=4, byteorder='little', signed=False)
            code += len(false_body).to_bytes(length=4, byteorder='little', signed=False)
            code += true_body
            code += false_body

            i += 2
        elif instr == 'ldf':
            if not isinstance(expr[i], int):
                raise AssembleError(f'Invalid first argument for ldf: {expr[i]}')
            if not isinstance(expr[i + 1], list):
                raise AssembleError(f'Invalid second argument for ldf: {expr[i+1]}')
            nargs = expr[i]
            body = expr[i + 1]
            i += 2
            code += bytes([0x43])
            code += nargs.to_bytes(length=4, byteorder='little', signed=True)
            body_code = _assemble(body, start_offset + len(code) + 4, strings, symbols)
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
        elif instr == 'ldstr':
            s = expr[i]
            i += 1
            if not isinstance(s, String):
                raise AssembleError(f'Invalid argument for ldstr: {s}')
            try:
                strnum = strings.index(s)
            except ValueError:
                strings.append(s)
                strnum = len(strings) - 1
            code += bytes([0x46])
            code += strnum.to_bytes(length=4, byteorder='little', signed=True)
        elif instr == 'strtab':
            raise AssembleError('Explicit symtab not allowed')
        elif instr == 'ldsym':
            sym = expr[i]
            i += 1
            if not isinstance(sym, Symbol):
                raise AssembleError(f'Invalid argument for ldstr: {sym}')
            try:
                symnum = symbols.index(sym)
            except ValueError:
                symbols.append(sym)
                symnum = len(symbols) - 1
            code += bytes([0x48])
            code += symnum.to_bytes(length=4, byteorder='little', signed=True)
        elif instr == 'symtab':
            raise AssembleError('Explicit symtab not allowed')
        elif instr == 'set':
            sym = expr[i]
            i += 1
            if not isinstance(sym, Symbol):
                raise AssembleError(f'Invalid argument type for set: {sym}')
            try:
                symnum = symbols.index(sym)
            except ValueError:
                symbols.append(sym)
                symnum = len(symbols) - 1
            code += bytes([0x4a])
            code += symnum.to_bytes(length=4, byteorder='little', signed=False)
        elif instr == 'get':
            sym = expr[i]
            i += 1
            if not isinstance(sym, Symbol):
                raise AssembleError(f'Invalid argument type for get: {sym}')
            try:
                symnum = symbols.index(sym)
            except ValueError:
                symbols.append(sym)
                symnum = len(symbols) - 1
            code += bytes([0x4b])
            code += symnum.to_bytes(length=4, byteorder='little', signed=False)
        else:
            raise AssembleError(f'Unknown instruction: {instr}')

    return code


def assemble(code: (Pair | list), start_offset: int = 0) -> bytes:
    strings = []
    symbols = []

    if not isinstance(code, (Pair, list)):
        raise AssembleError(f'Cannot assemble: {code}')

    if isinstance(code, Pair):
        code = code.to_list_recursive()

    assembled = _assemble(code, start_offset, strings, symbols)

    # add symbol strings
    for sym in symbols:
        sname = String(sym.name)
        if sname not in strings:
            strings.append(sname)

    strtab = b''
    if len(strings) > 0:
        strtab = bytes([0x47])  # strtab instruction
        strtab += len(strings).to_bytes(length=4, byteorder='little', signed=False)
        for s in strings:
            strtab += len(s).to_bytes(length=4, byteorder='little', signed=False)
            strtab += s.encode()

    symtab = b''
    if len(symbols) > 0:
        symtab = bytes([0x49])  # symtab instruction
        symtab += len(symbols).to_bytes(length=4, byteorder='little', signed=False)
        for sym in symbols:
            sname = String(sym.name)
            strnum = strings.index(sname)
            symtab += strnum.to_bytes(length=4, byteorder='little', signed=False)

    assembled = strtab + symtab + assembled

    return assembled


def configure_argparse(parser: argparse.ArgumentParser):
    parser.description = 'Assemble SECD instructions written in ' \
        's-expr form into a binary SECD module.'

    parser.add_argument(
        'input', default='-', nargs='?',
        help='Input file. Stdin is used if not specified or a dash (-) '
        'is passed instead. Defaults to reading from stdin.')

    parser.set_defaults(func=main)


def main(args):
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

    if expr is None:
        print('Input is empty.', file=sys.stderr)
        sys.exit(1)

    try:
        code = assemble(expr, 0)
    except AssembleError as e:
        print(f'Assemble error: {e}', file=sys.stderr)
        sys.exit(1)

    sys.stdout.buffer.write(code)
