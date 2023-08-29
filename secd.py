#!/usr/bin/env python3

from fractions import Fraction
import os
import struct
import sys
import argparse
import unicodedata

import runtime
from exceptions import RunError
from fasl import DbgInfoDefineRecord, DbgInfoExprRecord, Fasl
from snippet import show_snippet
from machinetypes import (
    Bool, Bytevector, Char, Complex, Float, Integer, List, Nil, Number, Pair, Port, Rational, String, Symbol, Procedure, Continuation, TrickType, Values, Vector, Void, WrappedValue,
)


class AbortedException(Exception):
    def __init__(self, message, continuation):
        self.message = message
        self.continuation = continuation

    def __str__(self):
        if self.continuation:
            return f'<AbortedException "{self.message}" with continuation>'
        else:
            return f'<AbortedException "{self.message}">'


class SystemContinuation(Continuation):
    def action(self, machine):
        raise NotImplementedError


class ExceptionReraiseContinuation(SystemContinuation):
    def __init__(self, python_exception: Exception, msg: str):
        self.python_exception = python_exception
        self.msg = msg

    def action(self, machine):
        # disable exception handler (likely installed by stdlib), otherwise
        # we could get caught up in an infinite loop
        machine.exception_handler = None

        raise RunError(self.msg) from self.python_exception


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
        if not isinstance(value, TrickType):
            raise ValueError(f'Invalid type being pushed: {repr(value)}')

        self.s.append(value)

    def pushx(self, value):
        self.s.append(value)

    def top(self, type=None, instr_name=None, arg_name=None):
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
                arg_desc = f' (for argument "{arg_name}")' if arg_name else ''
                raise RunError(f'{prefix}Reading a value{arg_desc} when zero is available.')
            else:
                prefix = ''
                if instr_name:
                    prefix = f'{instr_name}: '
                arg_desc = f' (for argument "{arg_name}")' if arg_name else ''
                raise RunError(f'{prefix}Reading a value{arg_desc} when multiple is available.')

        if type and not isinstance(value, type):
            prefix = ''
            if instr_name:
                prefix = f'{instr_name}: '
            arg_desc = f' "{arg_name}"' if arg_name else ''
            raise RunError(
                f'{prefix}Expected operand{arg_desc} of type "{type.__name__}", got: '
                f'{value}')

        return value

    def pop(self, type=None, instr_name=None, arg_name=None):
        value = self.top(type, instr_name, arg_name=arg_name)
        self.s.pop()
        return value

    def topx(self):
        return self.s[-1]

    def popx(self):
        return self.s.pop()

    def push_multiple(self, values):
        assert isinstance(values, list)
        self.s.append(Values(values))

    def pop_multiple(self, n=None, instr_name=None):
        if len(self.s) == 0:
            prefix = ''
            if instr_name:
                prefix = f'{instr_name}: '
            raise RunError(f'{prefix}Attempting to read from an empty stack')

        v = self.s.pop()
        if not isinstance(v, Values):
            v = Values([v])

        if n is not None and len(v) != n:
            prefix = ''
            if instr_name:
                prefix = f'{instr_name}: '
            raise RunError(
                f'{prefix}Reading {n} value(s), when {len(v)} is available.')

        return v

    def swap(self):
        self.s[-2], self.s[-1] = self.s[-1], self.s[-2]

    def extended(self, ls):
        assert isinstance(ls, list)
        return Stack(self.s + ls)

    def copy(self):
        return Stack([i for i in self.s])

    def clear(self):
        self.s = []


class Secd:
    def __init__(self, fasls=None):
        self.s = Stack()
        self.e = []
        self.c = 0
        self.d = []
        self.halt_code = None
        self.dummy_frame = object()
        self.debug = False
        self.symvals = {}
        self.cur_fasl = None
        self.exception_handler = None

        self.setup_instructions()
        self.setup_runtime()

        if fasls:
            self.load_fasls(fasls)

    def setup_instructions(self):
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
            0x0d: self.run_void,
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
            0x37: self.run_swap,
            0x38: self.run_mkvec,
            0x39: self.run_vecset,
            0x3a: self.run_vecref,
            0x3b: self.run_veclen,
            0x3c: self.run_wrap,
            0x3d: self.run_unwrap,
            0x3e: self.run_seh,
            0x3f: self.run_abort,
            0x40: self.run_mkbvec,
            0x41: self.run_bvecset,
            0x42: self.run_bvecref,
            0x43: self.run_bveclen,
            0x44: self.run_f2i,
            0x45: self.run_i2f,
            0x46: self.run_f2q,
            0x47: self.run_qnum,
            0x48: self.run_qden,
            0x49: self.run_cplx,
            0x4a: self.run_creal,
            0x4b: self.run_cimag,
            0x80: self.run_ldc,
            0x81: self.run_ld,
            0x82: self.run_sel,
            0x83: self.run_ldf,
            0x84: self.run_st,
            0x85: self.run_trap,
            0x86: self.run_ldstr,
            0x88: self.run_ldsym,
            0x8a: self.run_set,
            0x8b: self.run_get,
            0x8c: self.run_unset,
            0x8d: self.run_ldcf,
            0x8e: self.run_ldcq,
        }

    def setup_runtime(self):
        self.runtime_procs = {}
        for module_name, module_info in runtime.modules.items():
            module_object = module_info['class']()
            for proc_name, proc_info in module_info['procs'].items():
                def get_do_proc(proc_info, method, full_name):
                    def do_proc():
                        args = []
                        for arg_name, arg_type in proc_info['args'].items():
                            value = self.s.pop(arg_type,
                                               instr_name=full_name,
                                               arg_name=arg_name)
                            args.append(value)
                        ret = method(*args)
                        if not isinstance(ret, proc_info['return_type']):
                            raise RunError(
                                f'Expected runtime procedure {full_name}'
                                f' to return a value of type '
                                f'{proc_info["return_type"].__name__}, '
                                f'but got: {ret}')
                        self.s.pushx(ret)
                    return do_proc
                method = getattr(module_object, proc_name)
                key = (module_info['opcode']), proc_info['opcode']
                self.runtime_procs[key] = get_do_proc(
                    proc_info, method, f'#$/{module_name}/{proc_name}')

    def call_runtime_proc(self, module_opcode, proc_opcode):
        self.runtime_procs[module_opcode, proc_opcode]()

    def find_expr(self, fasl: Fasl, c):
        dbginfo = fasl.get_section('dbginfo')
        if dbginfo is None:
            return None

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

    def find_define(self, fasl: Fasl, c):
        dbginfo = fasl.get_section('dbginfo')
        if dbginfo is None:
            return None

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

    def print_stack_frame(self, i, fasl: Fasl, c):
        prefix = f'=[{i}]= '
        dbginfo = fasl.get_section('dbginfo')
        if not dbginfo:
            if fasl.filename:
                print(f'{prefix}<no debug info in fasl "{fasl.filename}">')
            else:
                print(f'{prefix}<no debug info in fasl>')
            return
        if len(dbginfo.records) == 0:
            if fasl.filename:
                print(f'{prefix}<debug info in "{fasl.filename}" is empty>')
            else:
                print(f'{prefix}<debug info is empty>')
            return

        source_file = dbginfo.get_source_file(c)
        if source_file:
            with open(source_file) as f:
                text = f.read()

            if fasl.filename is not None:
                fasl_modify_time = os.path.getmtime(fasl.filename)
                src_modify_time = os.path.getmtime(source_file)
                if src_modify_time > fasl_modify_time:
                    print(f'WARNING: source file {source_file} is '
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

        if text and expr:
            show_snippet(text, expr.src_start, expr.src_end)
        elif text:
            print(' <no matching expression found>')

    def print_stack_trace(self):
        self.print_stack_frame(0, self.cur_fasl, self.c - 1)

        i = 1
        for cont in reversed(self.d):
            if isinstance(cont, SystemContinuation):
                print(f'=[{i}]= <system continuation>')
                continue

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
        if isinstance(cont, SystemContinuation):
            cont.action(self)
            return

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

    def load_fasls(self, fasls):
        # there's no difference between loading and executing a fasl. the only
        # reason we have this function is that "loading" sounds nicer when
        # talking about libraries, and execute makes more sense for the main
        # program.
        for fasl in fasls:
            self.execute_fasl(fasl)

    def load_fasl(self, fasl):
        # see comments in load_fasls
        self.execute_fasl(fasl)

    def execute_fasl(self, fasl):
        self.cur_fasl = fasl
        self.e = []
        self.c = 0
        self.d = []
        while self.halt_code is None:
            try:
                opcode = self.cur_fasl.code[self.c]
            except IndexError:
                break

            self.c += 1

            try:
                func = self.op_funcs[opcode]
            except KeyError:
                # this will not be raised as a Trick exception, because at this
                # points its unlikely we can even call an exception handler.
                raise RunError(f'Invalid op-code: {opcode}')

            try:
                func()
            except AbortedException as e:
                # this happens when an exception is not handled. we first
                # restore the original continuation, if available, so that the
                # user can see where the actual error happened.
                if e.continuation:
                    # we'll pass a void value to the continuation, but that's
                    # just arbitrary, since we have no intention of actually
                    # continuing with the continuation!
                    self.resume_continuation(e.continuation, [Void()])
                raise RunError(e.message)
            except RunError as e:
                if isinstance(e, runtime.TrickExitException):
                    raise
                if self.exception_handler:
                    msg = String(str(e))
                    continuation = self.create_continuation()
                    kind = Bool(False)
                    if e.kind is not None:
                        kind = e.kind
                    args = List.from_list([msg, kind, continuation])
                    self.s.push(args)
                    self.s.push(self.exception_handler)

                    # add a continuation that would cause the exception to be
                    # re-raised if the handler ever returns. this should NOT
                    # happen, if it does, it's a bug in the stdlib exception
                    # handler, and this continuation is a guard to catch that.
                    # Notice that this exception handler is not the same as the
                    # Scheme concept of exception handler, which is implemented
                    # entirely in stdlib.
                    self.d.append(ExceptionReraiseContinuation(
                        e,
                        'System exception handler returned. This is '
                        'likely a bug in stdlib.'))

                    # call the handler as a tail call so that current
                    # continuation is not added to d.
                    self._do_apply('<eh>', tail_call=True)
                else:
                    raise

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
        result = Pair(car, cdr)
        self.s.pushx(result)
        if self.debug: print(f'cons {car} onto {cdr} => {result}')

    def run_ldc(self):
        value = self.cur_fasl.code[self.c:self.c+4]
        self.c += 4
        value = int.from_bytes(value, byteorder='little', signed=True)
        self.s.pushx(Integer(value))
        if self.debug: print(f'ldc {value}')

    def run_ldcf(self):
        value = self.cur_fasl.code[self.c:self.c+8]
        self.c += 8
        value, = struct.unpack('<d', value)
        self.s.pushx(Float(value))
        if self.debug: print(f'ldcf {value}')

    def run_ldcq(self):
        value = self.cur_fasl.code[self.c:self.c+16]
        self.c += 16
        numerator, denominator = struct.unpack('<qQ', value)
        frac = Fraction(numerator, denominator)
        self.s.pushx(Rational(frac))
        if self.debug: print(f'ldcq {value}')

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
        retval = self.s.pop_multiple(instr_name='join').as_list()
        self.resume_continuation(self.d.pop(), retvals=retval)
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
        proc = Procedure(
            self.c, self.e, self.cur_fasl,
            nparams=nparams, rest_param=rest_param)
        self.s.pushx(proc)
        self.c += body_size
        if self.debug: print(f'ldf body_size={body_size}')

    def fit_args(self, proc, args):
        """
        Check procedure arguments against args. If they mismatch,
        return an error, otherwise return them as a python list. In
        case the procedure has a rest parameter, any extra arguments
        are stored as a (Scheme) list in the last argument.
        """
        if proc.rest_param:
            if len(args) < proc.nparams:
                desc = 'procedure'
                proc_name = self.find_procedure_name(proc)
                if proc_name:
                    desc += f' "{proc_name}"'
                raise RunError(
                    f'Invalid number of arguments for {desc} (expected '
                    f'at least {proc.nparams}, got {len(args)})')
            args = args.to_list()
            rest = List.from_list(args[proc.nparams:])
            args = args[:proc.nparams] + [rest]
        else:
            if len(args) != proc.nparams:
                desc = 'procedure'
                proc_name = self.find_procedure_name(proc)
                if proc_name:
                    desc += f' "{proc_name}"'
                raise RunError(
                    f'Invalid number of arguments for {desc} (expected '
                    f'{proc.nparams}, got {len(args)})')

            args = args.to_list()

        return args

    def _do_apply(self, name, tail_call=False, dummy_frame=False):
        assert not tail_call or not dummy_frame

        proc = self.s.pop(Procedure, name)
        args = self.s.pop(List, name)
        if not args.is_proper():
            raise RunError(f'Argument list not proper: {args}')

        args = self.fit_args(proc, args)

        if dummy_frame:
            # code specific to "rap" instruction: replace an already existing
            # dummy frame.

            if proc.e[0] != self.dummy_frame:
                raise RunError('No dummy frame.')

            # note that we don't store e[0] on d, since it contains the dummy
            # frame. in normal 'ap' that does not exist, so we can store the
            # entire contents of e.
            self.d.append(self.create_continuation(e=self.e[1:]))

            # replace dummy frame with actual frame
            proc.e[0] = args
        elif not tail_call:
            self.d.append(self.create_continuation())
        else:
            # tail call: don't add a new continuation
            pass

        self.cur_fasl = proc.fasl

        if self.debug: print(f'{name} {self.c} => {proc.c}')

        if isinstance(proc, Continuation):
            # a continuation has only a "rest" argument
            rest_args = args[0]

            # resume the continuation, passing the rest args as the "return
            # values" it expects.
            self.resume_continuation(proc, rest_args.to_list())
        else:
            if dummy_frame:
                # we've already placed the arguments inside proc.e, so we
                # won't need to add the arguments to e here
                self.s, self.e, self.c = Stack(), proc.e, proc.c
            else:
                self.s, self.e, self.c = Stack(), [args] + proc.e, proc.c

    def run_ap(self):
        self._do_apply('ap', tail_call=False)

    def run_ret(self):
        retvals = self.s.pop_multiple().as_list()
        self.resume_continuation(self.d.pop(), retvals)
        if self.debug: print(f'ret retval={retvals}')

    def run_halt(self):
        self.halt_code = self.s.pop(int, 'halt')
        self.s.pushx(Nil())  # return nil
        if self.debug: print(f'halt {self.halt_code}')

    def run_iadd(self):
        arg1 = self.s.pop(Number, 'iadd')
        arg2 = self.s.pop(Number, 'iadd')
        self.s.pushx(arg2 + arg1)
        if self.debug: print(f'iadd {arg2} + {arg1}')

    def run_isub(self):
        arg1 = self.s.pop(Number, 'isub')
        arg2 = self.s.pop(Number, 'isub')
        self.s.pushx(arg1 - arg2)
        if self.debug: print(f'isub {arg1} - {arg2}')

    def run_imul(self):
        arg1 = self.s.pop(Number, 'imul')
        arg2 = self.s.pop(Number, 'imul')
        self.s.pushx(arg1 * arg2)
        if self.debug: print(f'imul {arg2} * {arg1}')

    def run_idiv(self):
        a = self.s.pop(Number, 'idiv')
        b = self.s.pop(Number, 'idiv')
        if b.is_zero():
            raise RunError('Division by zero')
        self.s.pushx(a / b)
        if self.debug: print(f'idiv {a} / {b}')

    def run_irem(self):
        a = self.s.pop(Number, 'irem')
        b = self.s.pop(Number, 'irem')
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
        arg1 = self.s.pop(Number, 'ilt')
        arg2 = self.s.pop(Number, 'ilt')
        try:
            self.s.pushx(Bool(True) if arg1 < arg2 else Bool(False))
        except ValueError as e:
            raise RunError(str(e))
        if self.debug: print(f'ilt {arg1} < {arg2}')

    def run_ile(self):
        arg1 = self.s.pop(Number, 'ile')
        arg2 = self.s.pop(Number, 'ile')
        try:
            self.s.pushx(Bool(True) if arg1 <= arg2 else Bool(False))
        except ValueError as e:
            raise RunError(str(e))
        if self.debug: print(f'ile {arg1} <= {arg2}')

    def run_dum(self):
        self.e = [self.dummy_frame] + self.e
        if self.debug: print(f'dum')

    def run_rap(self):
        self._do_apply('rap', dummy_frame=True)

    def run_tap(self):
        self._do_apply('tap', tail_call=True)

    def run_drop(self):
        value = self.s.pop_multiple(instr_name='drop')
        if self.debug: print(f'drop {value}')

    def run_xp(self):
        self.e[0].append(Void())  # expand frame by adding a void value to it
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
        elif isinstance(v, Integer):
            result = self.intern('int')
        elif isinstance(v, Rational):
            result = self.intern('rational')
        elif isinstance(v, Float):
            result = self.intern('float')
        elif isinstance(v, Complex):
            result = self.intern('complex')
        elif isinstance(v, String):
            result = self.intern('string')
        elif isinstance(v, Procedure):
            result = self.intern('procedure')
        elif isinstance(v, Bool):
            result = self.intern('bool')
        elif isinstance(v, Char):
            result = self.intern('char')
        elif isinstance(v, Vector):
            result = self.intern('vector')
        elif isinstance(v, Bytevector):
            result = self.intern('bytevector')
        elif isinstance(v, Port):
            result = self.intern('port')
        elif isinstance(v, Void):
            result = self.intern('void')
        elif isinstance(v, WrappedValue):
            result = v.type_id
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
            value = self.symvals[sym]
        except KeyError:
            raise RunError(f'Attempt to read unset symbol: {sym} ({symnum})')

        self.s.push(value)
        if self.debug: print(f'get {self.cur_fasl.symtab[symnum]} => {value}')

    def run_unset(self):
        symnum = self.cur_fasl.code[self.c:self.c+4]
        self.c += 4
        symnum = int.from_bytes(symnum, byteorder='little', signed=True)

        try:
            sym = self.cur_fasl.symtab[symnum]
        except IndexError:
            raise RunError(f'Invalid symbol number for "unset": {symnum}')

        del self.symvals[sym]
        if self.debug: print(f'unset {self.cur_fasl.symtab[symnum]}')

    def run_gensym(self):
        short_name = self.s.popx()
        sym = Symbol.gensym(short_name)
        self.s.pushx(sym)
        if self.debug: print(f'gensym2 {short_name} -- {sym}')

    def run_ccc(self): # call/cc
        proc = self.s.pop(Procedure, 'ccc')
        cont = self.create_continuation()
        args = List.from_list([cont])
        self.d.append(cont)

        self.s.pushx(args)
        self.s.pushx(proc)
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

    def run_mkvec(self):
        n = self.s.pop(Integer, 'mkvec')
        fill = self.s.pop(instr_name='mkvec')

        s = Vector([fill] * n)
        self.s.pushx(s)
        if self.debug: print(f'mkvec {n} * {fill}')

    def run_vecset(self):
        value = self.s.pop(instr_name='vecset')
        idx = self.s.pop(Integer, 'vecset')
        vector = self.s.pop(Vector, 'vecset')

        if 0 <= idx < len(vector):
            vector[idx] = value
        else:
            if len(vector) == 0:
                raise RunError(f'Invalid vector index: {idx} (vector is empty)')
            else:
                raise RunError(f'Invalid vector index: {idx} (expected: 0-{len(vector)-1})')

        if self.debug: print(f'vecset vec={vector} i={idx} val={value}')

    def run_vecref(self):
        vector = self.s.pop(Vector, 'vecref')
        idx = self.s.pop(Integer, 'vecref')

        if 0 <= idx < len(vector):
            value = vector[idx]
        else:
            if len(vector) == 0:
                raise RunError(f'Invalid vector index: {idx} (vector is empty)')
            else:
                raise RunError(f'Invalid vector index: {idx} (expected: 0-{len(vector)-1})')

        self.s.pushx(value)

        if self.debug: print(f'vecref v={vector} i={idx} => {value}')

    def run_veclen(self):
        vector = self.s.pop(Vector, 'veclen')
        result = Integer(len(vector))
        self.s.pushx(result)
        if self.debug: print(f'veclen v={vector} => {result}')

    def run_mkbvec(self):
        n = self.s.pop(Integer, 'mkbvec')
        fill = self.s.pop(Integer, instr_name='mkbvec')

        if fill < 0 or fill > 255:
            raise RunError(f'Invalid fill value for bytevector: {fill}')

        s = Bytevector([fill] * n)
        self.s.pushx(s)
        if self.debug: print(f'mkbvec {n} * {fill}')

    def run_bvecset(self):
        value = self.s.pop(Integer, 'bvecset')
        idx = self.s.pop(Integer, 'bvecset')
        bvector = self.s.pop(Bytevector, 'bvecset')

        if value < 0 or value > 255:
            raise RunError(f'Invalid element value for bytevector: {value}')

        if 0 <= idx < len(bvector):
            bvector[idx] = value
        else:
            if len(bvector) == 0:
                raise RunError(f'Invalid vector index: {idx} (vector is empty)')
            else:
                raise RunError(f'Invalid vector index: {idx} (expected: 0-{len(bvector)-1})')

        if self.debug: print(f'bvecset vec={bvector} i={idx} val={value}')

    def run_bvecref(self):
        bvector = self.s.pop(Bytevector, 'bvecref')
        idx = self.s.pop(Integer, 'bvecref')

        if 0 <= idx < len(bvector):
            value = bvector[idx]
        else:
            if len(bvector) == 0:
                raise RunError(f'Invalid bytevector index: {idx} (bytevector is empty)')
            else:
                raise RunError(f'Invalid bytevector index: {idx} (expected: 0-{len(bvector)-1})')

        self.s.pushx(value)

        if self.debug: print(f'bvecref v={bvector} i={idx} => {value}')

    def run_bveclen(self):
        bvector = self.s.pop(Bytevector, 'bveclen')
        result = Integer(len(bvector))
        self.s.pushx(result)
        if self.debug: print(f'bveclen v={bvector} => {result}')

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

        if self.debug: print(f'strset: idx={idx} char={char} => {s}')

    def run_strlen(self):
        s = self.s.pop(String, 'strlen')
        self.s.pushx(Integer(len(s)))
        if self.debug: print(f'strlen {s} => {len(s)}')

    def run_setcar(self):
        pair = self.s.pop(Pair, 'setcar')
        value = self.s.pop()

        pair.car = value
        if self.debug: print(f'setcar pair={pair} value={value}')

    def run_setcdr(self):
        pair = self.s.pop(Pair, 'setcdr')
        value = self.s.pop()

        pair.cdr = value
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

    def run_swap(self):
        self.s.swap()
        if self.debug: print('swap')

    def run_trap(self):
        runtime_opcodes = self.cur_fasl.code[self.c:self.c+2]
        self.c += 2
        runtime_opcodes = int.from_bytes(runtime_opcodes, byteorder='little', signed=True)

        module_opcode = runtime_opcodes & 0x00ff
        proc_opcode = (runtime_opcodes & 0xff00) >> 8
        self.call_runtime_proc(module_opcode, proc_opcode)
        if self.debug: print(f'trap module={module_opcode} proc={proc_opcode}')

    def run_void(self):
        self.s.pushx(Void())
        if self.debug: print('void')

    def run_wrap(self):
        value = self.s.popx()
        type_id = self.s.pop(Symbol, 'wrap')
        wrapped = WrappedValue(value, type_id)
        self.s.pushx(wrapped)
        if self.debug: print(f'wrap {value} in {type_id}')

    def run_unwrap(self):
        wrapped = self.s.pop(WrappedValue, 'unwrap')
        value = wrapped.value
        self.s.pushx(value)
        if self.debug: print(f'unwrap {value} from {wrapped.type_id}')

    def run_seh(self):
        proc = self.s.pop((Procedure, Bool), 'seh')
        if proc == Bool(False):
            self.exception_handler = None
        elif proc == Bool(True):
            raise RunError('Invalid exception handler: #t')
        else:
            if not proc.accepts_argument_count(3):
                raise RunError('Exception handler must accept three arguments (message, kind, and continuation)')
            self.exception_handler = proc
        if self.debug: print(f'seh: {proc}')

    def run_abort(self):
        message = self.s.popx()
        continuation = self.s.popx()
        if self.debug: print(f'abort {message}')

        if isinstance(message, String):
            message = message.value
        else:
            message = str(message)

        if not isinstance(continuation, Continuation):
            continuation = None

        raise AbortedException(message, continuation)

    def run_f2i(self):
        f = self.s.pop(Float, 'f2i')
        result = Integer(f)
        self.s.pushx(result)
        if self.debug: print(f'f2i {f} => {result}')

    def run_i2f(self):
        i = self.s.pop(Integer, 'i2f')
        result = Float(i)
        self.s.pushx(result)
        if self.debug: print(f'i2f {i} => {result}')

    def run_f2q(self):
        f = self.s.pop(Float, 'f2q')
        result = Rational(Fraction(f))
        self.s.pushx(result)
        if self.debug: print(f'f2q {f} => {result}')

    def run_qnum(self):
        q = self.s.pop((Rational, Integer), 'qnum')
        if isinstance(q, Integer):
            result = q
        else:
            result = Integer(q.frac.numerator)

        self.s.pushx(result)
        if self.debug: print(f'qnum {q} => {result}')

    def run_qden(self):
        q = self.s.pop((Rational, Integer), 'qden')
        if isinstance(q, Integer):
            result = Integer(1)
        else:
            result = Integer(q.frac.denominator)

        self.s.pushx(result)
        if self.debug: print(f'qden {q} => {result}')

    def run_cplx(self):
        imag = self.s.pop((Rational, Integer, Float), 'cplx')
        real = self.s.pop((Rational, Integer, Float), 'cplx')
        result = Complex(real, imag)
        self.s.pushx(result)
        if self.debug: print(f'cplx {result}')

    def run_creal(self):
        n = self.s.pop(Number, 'creal')
        if isinstance(n, Complex):
            result = n.real
        else:
            result = n
        self.s.pushx(result)
        if self.debug: print(f'creal {result}')

    def run_cimag(self):
        n = self.s.pop(Number, 'creal')
        if isinstance(n, Complex):
            result = n.imag
        else:
            # a zero of the same type as n
            result = type(n)(0)
        self.s.pushx(result)
        if self.debug: print(f'cimag {result}')

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

    m = Secd()
    m.load_fasls(lib_fasls)
    m.debug = args.debug
    try:
        m.execute_fasl(fasl)
    except runtime.TrickExitException as e:
        sys.exit(e.exit_code)
    except RunError as e:
        print('Run error:', e)
        m.print_stack_trace()
        sys.exit(1)

    if m.halt_code is None:
        print('Code exhausted.', file=sys.stderr, end=' ')
    else:
        print('Machine halted with code:', m.halt_code,
              file=sys.stderr, end=' ')

    print(f'({len(m.s)} item(s) left on the stack.)', file=sys.stderr)
