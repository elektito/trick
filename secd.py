#!/usr/bin/env python3

import os
import sys
import argparse
import unicodedata
from fasl import DbgInfoDefineRecord, DbgInfoExprRecord, Fasl
from snippet import show_snippet
from utils import format_user_error
from machinetypes import (
    Bool, Char, Integer, List, Nil, Pair, String, Symbol, Closure, Continuation, Values,
)


class RunError(Exception):
    pass


class UserError(RunError):
    pass


class Stack:
    def __init__(self, initial=None):
        if initial:
            self.s = initial
        else:
            self.s = []

    def __len__(self):
        return len(self.s)

    def __repr__(self):
        return f'<Stack {self.s}>'

    def push(self, value):
        if isinstance(value, Values):
            raise ValueError(
                'Attempting to write a Values object with "push"; use '
                'push_multiple.')
        if not isinstance(value, (Integer, Symbol, List, Char, String, Bool,
                                  Closure)):
            raise ValueError(f'Invalid type being pushed: {repr(value)}')

        self.s.append(value)

    def pushx(self, value):
        self.s.append(value)

    def top(self, type=None, instr_name=None):
        "read top of the stack without popping it"

        if len(self.s) == 0:
            prefix = ''
            if instr_name:
                prefix = f'{instr_name}: '
            raise RunError(f'{prefix}Attempting to read from an empty stack')

        value = self.s[-1]
        if isinstance(value, Values):
            if len(value) == 1:
                value = value[0]
            elif len(value) == 0:
                prefix = ''
                if instr_name:
                    prefix = f'{instr_name}: '
                raise RunError(f'{prefix}Reading a value when zero is available.')
            else:
                prefix = ''
                if instr_name:
                    prefix = f'{instr_name}: '
                raise RunError(f'{prefix}Reading a value when multiple is available.')

        if type and not isinstance(value, type):
            prefix = ''
            if instr_name:
                prefix = f'{instr_name}: '
            raise RunError(
                f'{prefix}Expected operand of type "{type.__name__}", got: '
                f'{value}')

        return value

    def pop(self, type=None, instr_name=None):
        value = self.top(type, instr_name)
        self.s.pop()
        return value

    def topx(self):
        return self.s[-1]

    def popx(self):
        return self.s.pop()

    def push_multiple(self, values):
        assert isinstance(values, list)
        self.s.append(Values(values))

    def pop_multiple(self, n=None):
        v = self.s.pop()
        if not isinstance(v, Values):
            v = Values([v])

        if n is not None and len(v) != n:
            raise RunError(
                f'Reading {n} value(s), when {len(v)} is available.')

        return v

    def extended(self, ls):
        assert isinstance(ls, list)
        return Stack(self.s + ls)

    def copy(self):
        return Stack([i for i in self.s])


class Secd:
    def __init__(self, program, libs):
        self.s = Stack()
        self.e = []
        self.c = 0
        self.d = []
        self.halt_code = None
        self.dummy_frame = object()
        self.debug = False
        self.symvals = {}

        assert isinstance(program, Fasl)
        assert all(isinstance(i, Fasl) for i in libs)
        self.program = program
        self.libs = libs
        self.libs_loaded = False
        self.cur_fasl = None

        self.op_funcs = {
            0x01: self.run_nil,
            0x02: self.run_true,
            0x03: self.run_false,
            0x04: self.run_cons,
            0x05: self.run_car,
            0x06: self.run_cdr,
            0x07: self.run_join,
            0x08: self.run_ap,
            0x09: self.run_ret,
            0x0a: self.run_tap,
            0x0b: self.run_dum,
            0x0c: self.run_rap,
            0x0d: self.run_print,
            0x0e: self.run_printc,
            0x0f: self.run_halt,
            0x10: self.run_iadd,
            0x11: self.run_isub,
            0x12: self.run_imul,
            0x13: self.run_idiv,
            0x14: self.run_irem,
            0x15: self.run_shr,
            0x16: self.run_shl,
            0x17: self.run_asr,
            0x18: self.run_bnot,
            0x19: self.run_band,
            0x1a: self.run_bor,
            0x1b: self.run_bxor,
            0x1c: self.run_ilt,
            0x1d: self.run_ile,
            0x1e: self.run_eq,
            0x1f: self.run_drop,
            0x20: self.run_dup,
            0x21: self.run_xp,
            0x22: self.run_type,
            0x23: self.run_error,
            0x24: self.run_gensym,
            0x25: self.run_ccc,
            0x26: self.run_i2ch,
            0x27: self.run_ch2i,
            0x28: self.run_ugcat,
            0x29: self.run_chup,
            0x2a: self.run_chdn,
            0x2b: self.run_chfd,
            0x2c: self.run_chdv,
            0x2d: self.run_mkstr,
            0x2e: self.run_strref,
            0x2f: self.run_strset,
            0x30: self.run_strlen,
            0x31: self.run_setcar,
            0x32: self.run_setcdr,
            0x33: self.run_m2l,
            0x34: self.run_l2m,
            0x35: self.run_sym2str,
            0x36: self.run_str2sym,
            0x40: self.run_ldc,
            0x41: self.run_ld,
            0x42: self.run_sel,
            0x43: self.run_ldf,
            0x44: self.run_st,
            0x46: self.run_ldstr,
            0x47: self.run_strtab,
            0x48: self.run_ldsym,
            0x49: self.run_symtab,
            0x4a: self.run_set,
            0x4b: self.run_get,
        }

    def find_expr(self, fasl, c):
        dbginfo = fasl.get_extra_section('dbginfo')
        matches = []
        for r in dbginfo.records:
            if not isinstance(r, DbgInfoExprRecord):
                continue
            if r.asm_start <= c < r.asm_end:
                matches.append(r)

        if len(matches) == 0:
            return None

        best = None
        for r in matches:
            if best is None or \
               (r.asm_end - r.asm_start) < (best.asm_end - best.asm_start):
                best = r

        return best

    def find_define(self, fasl, c):
        dbginfo = fasl.get_extra_section('dbginfo')
        matches = []
        for r in dbginfo.records:
            if not isinstance(r, DbgInfoDefineRecord):
                continue
            if r.asm_start <= c < r.asm_end:
                matches.append(r)

        if len(matches) == 0:
            return None

        best = None
        for r in matches:
            if best is None or \
               (r.asm_end - r.asm_start) < (best.asm_end - best.asm_start):
                best = r

        return best

    def print_stack_frame(self, i, fasl, c):
        prefix = f'=[{i}]= '
        dbginfo = fasl.get_extra_section('dbginfo')
        if not dbginfo:
            if fasl.filename:
                print(f'{prefix}<no debug info in fasl "{fasl.filename}">')
            else:
                print(f'{prefix}<no debug info in fasl>')
            return

        source_file = dbginfo.source_file
        if source_file:
            with open(source_file) as f:
                text = f.read()

            if fasl.filename is not None:
                fasl_modify_time = os.path.getmtime(fasl.filename)
                src_modify_time = os.path.getmtime(dbginfo.source_file)
                if src_modify_time > fasl_modify_time:
                    print(f'WARNING: source file {dbginfo.source_file} is '
                          f'newer than fasl {fasl.filename}')
        else:
            source_file = '<no source file>'
            text = ''

        expr = self.find_expr(fasl, c)
        define = self.find_define(fasl, c)

        if define:
            print(f'{prefix}{fasl.filename} ({source_file}) -- definition: {define.symbol_name}')
        else:
            print(f'{prefix}{fasl.filename} ({source_file})')

        if text:
            show_snippet(text, expr.src_start, expr.src_end)

    def print_stack_trace(self):
        self.print_stack_frame(0, self.cur_fasl, self.c - 1)

        i = 1
        for cont in reversed(self.d):
            # exclude continuations created by the "sel" instruction (for the
            # "if" primitive). these are not what one usually considers as part
            # of the stack trace.
            if cont.kind != 'sel':
                self.print_stack_frame(i, cont.fasl, cont.c - 1)
                i += 1

    def create_continuation(self, offset: int = 0, e=None, kind=None):
        return Continuation(
            self.s,
            self.e if e is None else e,
            self.c + offset,
            self.d,
            self.cur_fasl,
            kind=kind)

    def resume_continuation(self, cont: Continuation, retvals: list):
        self.cur_fasl = cont.fasl
        self.s = cont.s.copy()
        self.e = [i for i in cont.e]
        self.c = cont.c
        self.d = [i for i in cont.d]

        if retvals == []:
            self.s.push_multiple([])
        elif len(retvals) == 1:
            self.s.push(retvals[0])
        else:
            self.s.push_multiple(retvals)

    def load_libs(self):
        if self.libs_loaded:
            return
        for fasl in self.libs:
            self.execute_fasl(fasl)
        self.libs_loaded = True

    def run(self):
        self.load_libs()
        self.execute_fasl(self.program)

    def execute_fasl(self, fasl):
        self.cur_fasl = fasl
        self.c = 0
        while self.halt_code is None:
            try:
                opcode = self.cur_fasl.code[self.c]
            except IndexError:
                break

            self.c += 1

            try:
                func = self.op_funcs[opcode]
            except KeyError:
                raise RunError(f'Invalid op-code: {opcode}')

            func()

    def intern(self, name):
        sym = Symbol(name)
        if sym not in self.cur_fasl.symtab:
            sname = String(name)
            if sname not in self.cur_fasl.strtab:
                self.cur_fasl.strtab.append(sname)
            self.cur_fasl.symtab.append(sym)
        return sym

    def find_procedure_name(self, procedure):
        for sym, value in self.symvals.items():
            if value == procedure:
                break
        else:
            return None

        return sym.name

    def run_nil(self):
        self.s.pushx(Nil())
        if self.debug: print('nil')

    def run_cons(self):
        car = self.s.pop()
        cdr = self.s.pop()
        self.s.pushx(Pair(car, cdr))
        if self.debug: print(f'cons {car} onto {cdr}')

    def run_ldc(self):
        value = self.cur_fasl.code[self.c:self.c+4]
        self.c += 4
        value = int.from_bytes(value, byteorder='little', signed=True)
        self.s.pushx(Integer(value))
        if self.debug: print(f'ldc {value}')

    def run_ldstr(self):
        strnum = self.cur_fasl.code[self.c:self.c+4]
        self.c += 4
        strnum = int.from_bytes(strnum, byteorder='little', signed=True)
        s = self.cur_fasl.strtab[strnum]
        self.s.push(s)
        if self.debug: print(f'ldstr {s}')

    def run_ldsym(self):
        symnum = self.cur_fasl.code[self.c:self.c+4]
        self.c += 4
        symnum = int.from_bytes(symnum, byteorder='little', signed=True)
        try:
            s = self.cur_fasl.symtab[symnum]
        except IndexError:
            raise RunError(f'Invalid symbol index: {symnum} (symtab size: {len(self.cur_fasl.symtab)})')
        self.s.push(s)
        if self.debug: print(f'ldsym {s}')

    def run_ld(self):
        frame_idx = self.cur_fasl.code[self.c:self.c+2]
        index = self.cur_fasl.code[self.c+2:self.c+4]
        self.c += 4

        frame_idx = int.from_bytes(frame_idx, byteorder='little', signed=False)
        index = int.from_bytes(index, byteorder='little', signed=False)

        if frame_idx >= len(self.e):
            raise RunError(f'Invalid frame number: {frame_idx} (nframes: {len(self.e)})')
        frame = self.e[frame_idx]
        if frame == self.dummy_frame:
            raise RunError('Accessing dummy frame (possible cause: values in letrec bindings reading variables while being evaluated)')
        else:
            if index >= len(frame):
                raise RunError(f'Invalid variable index: {index} (frame size: {len(frame)})')
            value = frame[index]

        self.s.push(value)
        if self.debug: print(f'ld [{frame_idx}, {index}] -- frame={frame} value={value}')

    def run_st(self):
        frame_idx = self.cur_fasl.code[self.c:self.c+2]
        index = self.cur_fasl.code[self.c+2:self.c+4]
        self.c += 4

        frame_idx = int.from_bytes(frame_idx, byteorder='little', signed=False)
        index = int.from_bytes(index, byteorder='little', signed=False)

        value = self.s.pop()
        self.e[frame_idx][index] = value
        if self.debug: print(f'st {value} => [{frame_idx}, {index}]')

    def run_sel(self):
        true_len = self.cur_fasl.code[self.c:self.c+4]
        true_len = int.from_bytes(true_len, byteorder='little', signed=False)
        self.c += 4

        false_len = self.cur_fasl.code[self.c:self.c+4]
        false_len = int.from_bytes(false_len, byteorder='little', signed=False)
        self.c += 4

        cond = self.s.pop()
        self.d.append(self.create_continuation(
            offset=true_len + false_len,
            kind='sel'))
        if cond == Bool(False):
            self.c += true_len

        if self.debug: print(f'sel cond={cond}')

    def run_join(self):
        retval = self.s.top()
        self.resume_continuation(self.d.pop(), retvals=[retval])
        if self.debug: print(f'join')

    def run_ldf(self):
        nparams = self.cur_fasl.code[self.c:self.c+4]
        body_size = self.cur_fasl.code[self.c+4:self.c+8]
        nparams = int.from_bytes(nparams, byteorder='little', signed=True)
        body_size = int.from_bytes(body_size, byteorder='little', signed=False)
        self.c += 8
        if nparams < 0:
            rest_param = True
            nparams = -nparams - 1
        else:
            rest_param = False
        closure = Closure(
            self.c, self.e, self.cur_fasl,
            nparams=nparams, rest_param=rest_param)
        self.s.pushx(closure)
        self.c += body_size
        if self.debug: print(f'ldf body_size={body_size}')

    def fit_args(self, closure, args):
        """
        Check procedure arguments against args. If they mismatch,
        return an error, otherwise return them as a python list. In
        case the procedure has a rest parameter, any extra arguments
        are stored as a (Scheme) list in the last argument.
        """
        if closure.rest_param:
            if len(args) < closure.nparams:
                desc = 'procedure'
                proc_name = self.find_procedure_name(closure)
                if proc_name:
                    desc += f' "{proc_name}"'
                raise RunError(
                    f'Invalid number of arguments for {desc} (expected '
                    f'at least {closure.nparams}, got {len(args)})')
            args = args.to_list()
            rest = List.from_list(args[closure.nparams:])
            args = args[:closure.nparams] + [rest]
        else:
            if len(args) != closure.nparams:
                desc = 'procedure'
                proc_name = self.find_procedure_name(closure)
                if proc_name:
                    desc += f' "{proc_name}"'
                raise RunError(
                    f'Invalid number of arguments for {desc} (expected '
                    f'{closure.nparams}, got {len(args)})')

            args = args.to_list()

        return args

    def _do_apply(self, name, tail_call=False, dummy_frame=False):
        assert not tail_call or not dummy_frame

        closure = self.s.pop(Closure, name)
        args = self.s.pop(List, name)
        if not args.is_proper():
            raise RunError(f'Argument list not proper: {args}')

        if dummy_frame:
            # code specific to "rap" instruction: replace an already existing
            # dummy frame.

            if closure.e[0] != self.dummy_frame:
                raise RunError('No dummy frame.')

            # note that we don't store e[0] on d, since it contains the dummy
            # frame. in normal 'ap' that does not exist, so we can store the
            # entire contents of e.
            self.d.append(self.create_continuation(offset=0, e=self.e[1:]))

            # replace dummy frame with actual frame
            closure.e[0] = args
        elif not tail_call:
            self.d.append(self.create_continuation())
        else:
            # tail call: don't add a new continuation
            pass

        self.cur_fasl = closure.fasl

        if self.debug: print(f'{name} {self.c} => {closure.c}')

        args = self.fit_args(closure, args)
        if isinstance(closure, Continuation):
            rest_args = args[0]
            v = Values(rest_args.to_list())
            self.s = closure.s.extended([v])
            self.e = [i for i in closure.e]
            self.c = closure.c
            self.d = [i for i in closure.d]
        else:
            if dummy_frame:
                # we've already placed the arguments inside closure.e, so we
                # won't need to add the arguments to e here
                self.s, self.e, self.c = Stack(), closure.e, closure.c
            else:
                self.s, self.e, self.c = Stack(), [args] + closure.e, closure.c

    def run_ap(self):
        self._do_apply('ap', tail_call=False)

    def run_ret(self):
        retvals = self.s.pop_multiple().as_list()
        self.resume_continuation(self.d.pop(), retvals)
        if self.debug: print(f'ret retval={retvals}')

    def run_print(self):
        v = self.s.topx()  # leave the value on stack as return value
        if self.debug: print(f'print {v}')
        print_value(v)

    def run_printc(self):
        n = self.s.topx()  # leave the value on stack as return value
        if self.debug: print(f'printc {n}')
        print(chr(n), end='')

    def run_halt(self):
        self.halt_code = self.s.pop(int, 'halt')
        self.s.pushx(Nil())  # return nil
        if self.debug: print(f'halt {self.halt_code}')

    def run_iadd(self):
        arg1 = self.s.pop(Integer, 'iadd')
        arg2 = self.s.pop(Integer, 'iadd')
        self.s.pushx(arg2 + arg1)
        if self.debug: print(f'iadd {arg2} + {arg1}')

    def run_isub(self):
        arg1 = self.s.pop(Integer, 'isub')
        arg2 = self.s.pop(Integer, 'isub')
        self.s.pushx(arg1 - arg2)
        if self.debug: print(f'isub {arg1} - {arg2}')

    def run_imul(self):
        arg1 = self.s.pop(Integer, 'imul')
        arg2 = self.s.pop(Integer, 'imul')
        self.s.pushx(arg1 * arg2)
        if self.debug: print(f'imul {arg2} * {arg1}')

    def run_idiv(self):
        a = self.s.pop(Integer, 'idiv')
        b = self.s.pop(Integer, 'idiv')
        if b == 0:
            raise RunError('Division by zero')
        self.s.pushx(a // b)
        if self.debug: print(f'idiv {a} / {b}')

    def run_irem(self):
        a = self.s.pop(Integer, 'irem')
        b = self.s.pop(Integer, 'irem')
        if b == 0:
            raise RunError('Division by zero')

        result = abs(a) % abs(b) * (1,-1)[a < 0]

        self.s.pushx(result)
        if self.debug: print(f'irem {a} % {b}')

    def run_shr(self):
        n = self.s.pop(Integer, 'shr')
        shift = self.s.pop(Integer, 'shr')
        self.s.pushx((n % 0x100000000) >> shift)  # assuming 32 bits
        if self.debug: print(f'shr {n} >> {shift}')

    def run_shl(self):
        n = self.s.pop(Integer, 'shl')
        shift = self.s.pop(Integer, 'shl')
        self.s.pushx(n << shift)
        if self.debug: print(f'shl {n} << {shift}')

    def run_asr(self):
        n = self.s.pop(Integer, 'asr')
        shift = self.s.pop(Integer, 'asr')
        self.s.pushx(n >> shift)
        if self.debug: print(f'asr {n} >> {shift}')

    def run_bnot(self):
        n = self.s.pop(Integer, 'bnot')
        self.s.pushx(~n)
        if self.debug: print(f'bnot {n} => {~n}')

    def run_band(self):
        n1 = self.s.pop(Integer, 'band')
        n2 = self.s.pop(Integer, 'band')
        self.s.pushx(n1 & n2)
        if self.debug: print(f'asr {n1} & {n2}')

    def run_bor(self):
        n1 = self.s.pop(Integer, 'bor')
        n2 = self.s.pop(Integer, 'bor')
        self.s.pushx(n1 | n2)
        if self.debug: print(f'bor {n1} | {n2}')

    def run_bxor(self):
        n1 = self.s.pop(Integer, 'bxor')
        n2 = self.s.pop(Integer, 'bxor')
        self.s.pushx(n1 ^ n2)
        if self.debug: print(f'bxor {n1} | {n2}')

    def run_ilt(self):
        arg1 = self.s.pop(Integer, 'ilt')
        arg2 = self.s.pop(Integer, 'ilt')
        self.s.pushx(Bool(True) if arg1 < arg2 else Bool(False))
        if self.debug: print(f'ilt {arg1} < {arg2}')

    def run_ile(self):
        arg1 = self.s.pop(Integer, 'ile')
        arg2 = self.s.pop(Integer, 'ile')
        self.s.pushx(Bool(True) if arg1 <= arg2 else Bool(False))
        if self.debug: print(f'ile {arg1} <= {arg2}')

    def run_dum(self):
        self.e = [self.dummy_frame] + self.e
        if self.debug: print(f'dum')

    def run_rap(self):
        self._do_apply('rap', dummy_frame=True)

    def run_tap(self):
        self._do_apply('tap', tail_call=True)

    def run_drop(self):
        value = self.s.pop_multiple()
        if self.debug: print(f'drop {value}')

    def run_xp(self):
        self.e[0].append([])  # expand frame by adding a nil value to it
        if self.debug: print(f'xp')

    def run_dup(self):
        self.s.pushx(self.s.topx())
        if self.debug: print(f'dup {self.s.topx()}')

    def run_true(self):
        self.s.pushx(Bool(True))
        if self.debug: print(f'true')

    def run_false(self):
        self.s.pushx(Bool(False))
        if self.debug: print(f'false')

    def run_strtab(self):
        nstrs = self.cur_fasl.code[self.c:self.c+4]
        self.c += 4
        nstrs = int.from_bytes(nstrs, byteorder='little', signed=False)
        self.cur_fasl.strtab = []
        for _ in range(nstrs):
            length = self.cur_fasl.code[self.c:self.c+4]
            self.c += 4
            length = int.from_bytes(length, byteorder='little', signed=False)
            s = self.cur_fasl.code[self.c:self.c+length]
            s = String.from_bytes(s)
            self.cur_fasl.strtab.append(s)
            self.c += length

        if self.debug: print(f'strtab {nstrs}')

    def run_symtab(self):
        if self.cur_fasl.symtab != []:
            raise RunError('Multiple symtab instructions')
        nsyms = self.cur_fasl.code[self.c:self.c+4]
        self.c += 4
        nsyms = int.from_bytes(nsyms, byteorder='little', signed=False)
        strnums = []
        for _ in range(nsyms):
            strnum = self.cur_fasl.code[self.c:self.c+4]
            self.c += 4
            strnum = int.from_bytes(strnum, byteorder='little', signed=False)
            strnums.append(strnum)

        self.cur_fasl.symtab = [Symbol(self.cur_fasl.strtab[i].value) for i in strnums]
        if self.debug: print(f'symtab {nsyms}')

    def run_car(self):
        pair = self.s.pop(Pair, 'car')
        car = pair.car
        self.s.pushx(car)
        if self.debug: print(f'car => {car}')

    def run_cdr(self):
        pair = self.s.pop(Pair, 'cdr')
        cdr = pair.cdr
        self.s.pushx(cdr)
        if self.debug: print(f'cdr => {cdr}')

    def run_type(self):
        v = self.s.popx()
        if v == Nil():
            result = self.intern('nil')
        elif isinstance(v, Symbol):
            result = self.intern('symbol')
        elif isinstance(v, Pair):
            result = self.intern('pair')
        elif isinstance(v, int):
            result = self.intern('int')
        elif isinstance(v, String):
            result = self.intern('string')
        elif isinstance(v, Closure):
            result = self.intern('closure')
        elif isinstance(v, Bool):
            result = self.intern('bool')
        elif isinstance(v, Char):
            result = self.intern('char')
        else:
            raise RunError(f'Unknown type: {v}')
        self.s.pushx(result)
        if self.debug: print(f'type {type(v)} => {result}')

    def run_eq(self):
        x = self.s.pop()
        y = self.s.pop()
        result = Bool(x == y)
        self.s.pushx(result)
        if self.debug: print(f'eq {result}')

    def run_set(self):
        symnum = self.cur_fasl.code[self.c:self.c+4]
        self.c += 4
        symnum = int.from_bytes(symnum, byteorder='little', signed=True)

        try:
            sym = self.cur_fasl.symtab[symnum]
        except IndexError:
            raise RunError(f'Invalid symbol number for "set": {symnum}')

        value = self.s.pop()
        self.symvals[sym] = value

        if self.debug: print(f'set {sym} => {value}')

    def run_get(self):
        symnum = self.cur_fasl.code[self.c:self.c+4]
        self.c += 4
        symnum = int.from_bytes(symnum, byteorder='little', signed=True)

        try:
            sym = self.cur_fasl.symtab[symnum]
        except IndexError:
            raise RunError(f'Invalid symbol number for "get": {symnum}')

        try:
            # leave the set value on the stack as the return value of set
            value = self.symvals[sym]
        except KeyError:
            raise RunError(f'Attempt to read unset symbol: {sym} ({symnum})')

        self.s.push(value)
        if self.debug: print(f'get {self.cur_fasl.symtab[symnum]} => {value}')

    def run_error(self):
        if self.debug: print(f'error')
        raise UserError('User error')

    def run_gensym(self):
        sym = Symbol.gensym()
        self.s.pushx(sym)
        if self.debug: print(f'gensym {sym}')

    def run_ccc(self): # call/cc
        closure = self.s.pop(Closure, 'ccc')
        cont = self.create_continuation()
        args = List.from_list([cont])
        self.d.append(cont)

        self.s.pushx(args)
        self.s.pushx(closure)
        self._do_apply('ccc')

    def run_i2ch(self):
        char_code = self.s.pop(Integer, 'i2ch')
        self.s.pushx(Char(char_code))
        if self.debug: print(f'i2ch {char_code}')

    def run_ch2i(self):
        char = self.s.pop(Char, 'ch2i')
        self.s.pushx(Integer(char.char_code))
        if self.debug: print(f'ch2i {char}')

    def run_ugcat(self):  # unicode general category
        char = self.s.pop(Char, 'ugcat')
        cat = unicodedata.category(chr(char.char_code))
        sym = self.intern(cat)
        self.s.pushx(sym)
        if self.debug: print(f'ugcat {char}')

    def run_chup(self):
        char = self.s.pop(Char, 'chup')
        new_char = Char(ord(chr(char.char_code).upper()))
        self.s.pushx(new_char)
        if self.debug: print(f'chup {char}')

    def run_chdn(self):
        char = self.s.pop(Char, 'chdn')
        new_char = Char(ord(chr(char.char_code).lower()))
        self.s.pushx(new_char)
        if self.debug: print(f'chdn {char}')

    def run_chfd(self):
        char = self.s.pop(Char, 'chfd')
        folded_char = chr(char.char_code).casefold()
        if len(folded_char) > 1:
            # for example: german letter ß becomes "ss" when folded. not sure
            # what else to do besides defaulting to lower case. chez scheme also
            # seems to be doing the same thing.
            folded_char = chr(char.char_code).lower()
        new_char = Char(ord(folded_char))
        self.s.pushx(new_char)
        if self.debug: print(f'chfd {char}')

    def run_chdv(self):
        char = self.s.pop(Char, 'chdv')
        digit_value = unicodedata.digit(chr(char.char_code), None)
        if digit_value is None:
            digit_value = Bool(False)
        else:
            digit_value = Integer(digit_value)
        self.s.pushx(digit_value)
        if self.debug: print(f'chdv {char}')

    def run_mkstr(self):
        nchars = self.s.pop(Integer, 'mkstr')
        fill_char = self.s.pop(Char, 'mkstr')

        s = chr(fill_char.char_code) * nchars
        s = String(s)
        self.s.pushx(s)
        if self.debug: print(f'mkstr {nchars} * {fill_char}')

    def run_strref(self):
        s = self.s.pop(String, 'strref')
        idx = self.s.pop(Integer, 'strref')

        try:
            c = s.value[idx]
        except IndexError:
            raise RunError(f'Invalid index {idx} for string {s}')

        c = Char(ord(c))
        self.s.pushx(c)
        if self.debug: print(f'strref: s={s} idx={idx} => {c}')

    def run_strset(self):
        s = self.s.pop(String, 'strset')
        idx = self.s.pop(Integer, 'strset')
        char = self.s.pop(Char, 'strset')

        if idx < 0 or idx >= len(s):
            raise RunError(f'Invalid index {idx} for string {s}')

        s.value = s.value[:idx] + chr(char.char_code) + s.value[idx + 1:]

        self.s.pushx(s)
        if self.debug: print(f'strset: idx={idx} char={char} => {s}')

    def run_strlen(self):
        s = self.s.pop(String, 'strlen')
        self.s.pushx(Integer(len(s)))
        if self.debug: print(f'strlen {s} => {len(s)}')

    def run_setcar(self):
        pair = self.s.pop(Pair, 'setcar')
        value = self.s.pop()

        pair.car = value
        self.s.pushx(pair)
        if self.debug: print(f'setcar pair={pair} value={value}')

    def run_setcdr(self):
        pair = self.s.pop(Pair, 'setcdr')
        value = self.s.pop()

        pair.cdr = value
        self.s.pushx(pair)
        if self.debug: print(f'setcdr pair={pair} value={value}')

    def run_m2l(self):
        v = self.s.pop_multiple()
        result = List.from_list(v.as_list())
        self.s.pushx(result)
        if self.debug: print(f'm2l')

    def run_l2m(self):
        v = self.s.pop(List, 'l2m')
        result = Values(v.to_list())
        self.s.pushx(result)
        if self.debug: print(f'l2m')

    def run_sym2str(self):
        sym = self.s.pop(Symbol, 'sym2str')
        self.s.pushx(String(sym.name))
        if self.debug: print(f'sym2str {sym}')

    def run_str2sym(self):
        name = self.s.pop(String, 'str2sym')
        self.s.pushx(self.intern(name.value))
        if self.debug: print(f'str2sym {name}')


def configure_argparse(parser: argparse.ArgumentParser):
    parser.description = 'Run a binary SECD program'

    parser.add_argument(
        'input', default='-', nargs='?',
        help='Input FASL file. Stdin is used if not specified or a '
        'dash (-) is passed instead. Defaults to reading from stdin.')

    parser.add_argument(
        '--lib', '-l', action='append', default=[],
        help='Add a FASL to be loaded as a library.')

    parser.add_argument(
        '--debug', '-g', action='store_true', default=False,
        help='Enable debug mode.')

    parser.set_defaults(func=main)


def main(args):
    if args.input == '-':
        fasl = Fasl.load(sys.stdin.buffer)
    else:
        with open(args.input, 'rb') as f:
            fasl = Fasl.load(f, args.input)

    lib_fasls = []
    for lib in args.lib:
        with open(lib, 'rb') as f:
            lib_fasls.append(Fasl.load(f, lib))

    m = Secd(fasl, lib_fasls)
    m.debug = args.debug
    try:
        m.run()
    except UserError:
        err = m.s.pop()
        msg = format_user_error(err)
        print(f'Run error: {msg}', file=sys.stderr)
        m.print_stack_trace()
        sys.exit(1)
    except RunError as e:
        print('Run error:', e)
        m.print_stack_trace()
        sys.exit(1)

    if m.halt_code is None:
        print('Code exhausted.', file=sys.stderr)
    else:
        print('Machine halted with code:', m.halt_code, file=sys.stderr)


def print_value(v):
    if isinstance(v, String):
        print(v.value)
    elif isinstance(v, (Integer, Symbol, Bool, List, Closure, Char)):
        print(v)
    else:
        raise RunError(f'Unknown type to print: {v}')
