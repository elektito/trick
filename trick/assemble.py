#!/usr/bin/env python3

import io
import struct
import sys
import argparse

from . import runtime
from .exceptions import AssembleError
from .program import Program
from .fasl import DbgInfoDefineRecord, DbgInfoExprRecord, DbgInfoSourceFileRecord, Fasl, FaslDbgInfoSection, FaslLibInfoSection
from .read import Reader, ReadError
from .machinetypes import List, Rational, String, Symbol


class Assembler:
    def _assemble(self, expr, fasl: Fasl, offset: int,
                  dbg_records: list) -> bytes:
        if not isinstance(expr, list):
            raise AssembleError('Input not a list')

        dbg_expr_start_stack = []
        dbg_define_start_stack = []
        dbg_filename_start_stack = []

        i = 0
        code = b''
        while i < len(expr):
            instr = expr[i]
            i += 1

            if not isinstance(instr, Symbol):
                raise AssembleError(f'Instruction not a symbol: {instr}')

            if instr.name == ':expr-start':
                src_start = expr[i]
                i += 1
                asm_start = offset + len(code)
                dbg_expr_start_stack.append((src_start, asm_start))
                continue
            elif instr.name == ':expr-end':
                src_end = expr[i]
                i += 1
                src_start, asm_start = dbg_expr_start_stack.pop()
                if src_start is None or src_end is None:
                    continue
                asm_end = offset + len(code)
                dbg_records.append(
                    DbgInfoExprRecord(
                        src_start,
                        src_end,
                        asm_start,
                        asm_end))
                continue
            elif instr.name == ':define-start':
                defined_sym = expr[i]
                i += 1
                src_start = expr[i]
                i += 1
                asm_start = offset + len(code)
                dbg_define_start_stack.append((defined_sym, src_start, asm_start))
                continue
            elif instr.name == ':define-end':
                src_end = expr[i]
                i += 1
                defined_sym, src_start, asm_start = dbg_define_start_stack.pop()
                asm_end = offset + len(code)
                if src_start is None or src_end is None:
                    continue
                dbg_records.append(
                    DbgInfoDefineRecord(
                        defined_sym,
                        src_start,
                        src_end,
                        asm_start,
                        asm_end))
                continue
            elif instr.name == ':filename-start':
                filename = expr[i]
                i += 1
                asm_start = offset + len(code)
                dbg_filename_start_stack.append((filename, asm_start))
                continue
            elif instr.name == ':filename-end':
                filename, asm_start = dbg_filename_start_stack.pop()
                asm_end = offset + len(code)
                dbg_records.append(
                    DbgInfoSourceFileRecord(
                        filename,
                        asm_start,
                        asm_end))
                continue

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
                'void': 0x0d,
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
                'sym2str': 0x35,
                'str2sym': 0x36,
                'swap': 0x37,
                'mkvec': 0x38,
                'vecset': 0x39,
                'vecref': 0x3a,
                'veclen': 0x3b,
                'wrap': 0x3c,
                'unwrap': 0x3d,
                'seh': 0x3e,
                'abort': 0x3f,
                'mkbvec': 0x40,
                'bvecset': 0x41,
                'bvecref': 0x42,
                'bveclen': 0x43,
                'f2i': 0x44,
                'i2f': 0x45,
                'f2q': 0x46,
                'qnum': 0x47,
                'qden': 0x48,
                'cplx': 0x49,
                'creal': 0x4a,
                'cimag': 0x4b,
                'igt': 0x4c,
                'ige': 0x4d,
                'neg': 0x4e,
                'abs': 0x4f,
            }
            opcode = single_byte_instrs.get(instr)
            if opcode is not None:
                code += bytes([opcode])
            elif instr == 'ldc':
                value = expr[i]
                i += 1
                if not isinstance(value, int):
                    raise AssembleError(f'Invalid argument for ldc: {value}')
                code += bytes([0x80])
                code += value.to_bytes(length=4, byteorder='little', signed=True)
            elif instr == 'ld':
                if not isinstance(expr[i], list) or \
                   len(expr[i]) != 2 or \
                   not isinstance(expr[i][0], int) or \
                   not isinstance(expr[i][1], int):
                    raise AssembleError(f'Invalid argument for ld: {expr[i]}')
                frame, index = expr[i]
                i += 1
                code += bytes([0x81])
                code += frame.to_bytes(length=2, byteorder='little', signed=False)
                code += index.to_bytes(length=2, byteorder='little', signed=False)
            elif instr == 'sel':
                if not isinstance(expr[i], list) :
                    raise AssembleError(f'Invalid first argument for sel: {expr[i]}')
                if not isinstance(expr[i + 1], list):
                    raise AssembleError(f'Invalid second argument for sel: {expr[i+1]}')

                code += bytes([0x82])

                true_body = self._assemble(
                    expr[i], fasl, offset + len(code) + 8, dbg_records)
                false_body = self._assemble(
                    expr[i + 1], fasl,
                    offset + len(code) + 8 + len(true_body),
                    dbg_records)

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
                code += bytes([0x83])
                code += nargs.to_bytes(length=4, byteorder='little', signed=True)
                body_code = self._assemble(
                    body, fasl, offset + len(code) + 4, dbg_records)
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
                code += bytes([0x84])
                code += frame.to_bytes(length=2, byteorder='little', signed=False)
                code += index.to_bytes(length=2, byteorder='little', signed=False)
            elif instr == 'trap':
                name_sym = expr[i]
                i += 1
                module_name, proc_name = name_sym.name.split('/')
                if module_name not in runtime.modules:
                    raise AssembleError(f'Unknown runtime module: {module_name}')
                proc_desc = runtime.find_proc(module_name, proc_name)
                if proc_desc is None:
                    raise AssembleError(f'Unknown runtime procedure: {module_name}/{proc_name}')
                code += bytes([0x85])
                code += bytes([proc_desc['module_opcode']])
                code += bytes([proc_desc['opcode']])
            elif instr == 'ldstr':
                s = expr[i]
                i += 1
                if not isinstance(s, String):
                    raise AssembleError(f'Invalid argument for ldstr: {s}')
                strnum = fasl.add_string(s)
                code += bytes([0x86])
                code += strnum.to_bytes(length=4, byteorder='little', signed=True)
            elif instr == 'ldsym':
                sym = expr[i]
                i += 1
                if not isinstance(sym, Symbol):
                    raise AssembleError(f'Invalid argument for ldstr: {sym}')
                symnum = fasl.add_symbol(sym)
                code += bytes([0x88])
                code += symnum.to_bytes(length=4, byteorder='little', signed=True)
            elif instr == 'set':
                sym = expr[i]
                i += 1
                if not isinstance(sym, Symbol):
                    raise AssembleError(f'Invalid argument type for set: {sym}')
                symnum = fasl.add_symbol(sym)
                code += bytes([0x8a])
                code += symnum.to_bytes(length=4, byteorder='little', signed=False)
            elif instr == 'get':
                sym = expr[i]
                i += 1
                if not isinstance(sym, Symbol):
                    raise AssembleError(f'Invalid argument type for get: {sym}')
                symnum = fasl.add_symbol(sym)
                code += bytes([0x8b])
                code += symnum.to_bytes(length=4, byteorder='little', signed=False)
            elif instr == 'unset':
                sym = expr[i]
                i += 1
                if not isinstance(sym, Symbol):
                    raise AssembleError(f'Invalid argument type for unset: {sym}')
                symnum = fasl.add_symbol(sym)
                code += bytes([0x8c])
                code += symnum.to_bytes(length=4, byteorder='little', signed=False)
            elif instr == 'ldcf':
                value = expr[i]
                i += 1
                if not isinstance(value, float):
                    raise AssembleError(f'Invalid argument for ldcf: {value}')
                code += bytes([0x8d])
                code += struct.pack('<d', value)
            elif instr == 'ldcq':
                value = expr[i]
                i += 1
                if not isinstance(value, Rational):
                    raise AssembleError(f'Invalid argument for ldcq: {value}')
                code += bytes([0x8e])
                code += struct.pack('<qQ', value.frac.numerator, value.frac.denominator)
            else:
                raise AssembleError(f'Unknown instruction: {instr}')

        return code

    def assemble(self, code: (List | list), fasl: Fasl, *, dbg_info=False):
        if not isinstance(code, (List, list)):
            raise AssembleError(f'Cannot assemble: {code}')

        if isinstance(code, List):
            code = code.to_list_recursive()

        dbg_records = []
        assembled = self._assemble(code, fasl, len(fasl.code), dbg_records)
        fasl.code += assembled

        if dbg_info:
            section = fasl.get_section('dbginfo')
            if section is None:
                section = FaslDbgInfoSection()
                fasl.add_section(section)
            for r in dbg_records:
                section.add_record(r)

        return assembled

    def assemble_program(self, program: Program) -> Fasl:
        fasl = Fasl()
        self.assemble(program.code, fasl, dbg_info=program.debug_info_enabled)

        if program.defined_libs:
            libs_section = FaslLibInfoSection()
            for lib in program.defined_libs:
                libs_section.add_library(lib)
            fasl.add_section(libs_section)

        return fasl


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

    input = io.StringIO(text)
    reader = Reader(input)
    assembler = Assembler()

    try:
        expr = reader.read()
    except ReadError as e:
        print(f'Read error: {e}', file=sys.stderr)
        sys.exit(1)

    if expr is None:
        print('Input is empty.', file=sys.stderr)
        sys.exit(1)

    if not isinstance(expr, List):
        print('Input not a list')
        sys.exit(1)

    try:
        code = assembler.assemble(expr)
    except AssembleError as e:
        print(f'Assemble error: {e}', file=sys.stderr)
        sys.exit(1)

    sys.stdout.buffer.write(code)
