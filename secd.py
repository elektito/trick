#!/usr/bin/env python3

import sys
import argparse
import unicodedata
from utils import format_user_error
from machinetypes import (
    Bool, Char, List, Nil, Pair, String, Symbol, Closure, Continuation,
)


class RunError(Exception):
    pass


class UserError(RunError):
    pass


class Secd:
    def __init__(self, code):
        self.code = code

        self.s = []
        self.e = []
        self.c = 0
        self.d = []
        self.halt_code = None
        self.dummy_frame = object()
        self.debug = False
        self.strtab = [String('')]
        self.symtab = []
        self.symvals = {}

    def create_continuation(self, offset: int = 0, e=None):
        return Continuation(
            self.s,
            self.e if e is None else e,
            self.c + offset,
            self.d)

    def resume_continuation(self, cont: Continuation, retvals: list):
        self.s = cont.s
        self.e = cont.e
        self.c = cont.c

        if retvals == []:
            # maybe we'll make this meaningful in the future, but not for now
            raise NotImplementedError
        elif len(retvals) == 1:
            self.s.append(retvals[0])
        else:
            # multiple values...not implemented yet
            raise NotImplementedError

    def run(self):
        funcs = {
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
        code_len = len(self.code)
        while self.c < code_len and self.halt_code is None:
            opcode = self.code[self.c]
            self.c += 1

            try:
                func = funcs[opcode]
            except KeyError:
                raise RunError(f'Invalid op-code: {opcode}')

            func()

    def intern(self, name):
        sym = Symbol(name)
        if sym not in self.symtab:
            sname = String(name)
            if sname not in self.strtab:
                self.strtab.append(sname)
            self.symtab.append(sym)
        return sym

    def find_procedure_name(self, procedure):
        for symnum, value in self.symvals.items():
            if value == procedure:
                break
        else:
            return None

        sym = self.symtab[symnum]
        return sym.name

    def run_nil(self):
        self.s.append(Nil())
        if self.debug: print('nil')

    def run_cons(self):
        car = self.s.pop()
        cdr = self.s.pop()
        self.s.append(Pair(car, cdr))
        if self.debug: print(f'cons {car} onto {cdr}')

    def run_ldc(self):
        value = self.code[self.c:self.c+4]
        self.c += 4
        value = int.from_bytes(value, byteorder='little', signed=True)
        self.s.append(value)
        if self.debug: print(f'ldc {value}')

    def run_ldstr(self):
        strnum = self.code[self.c:self.c+4]
        self.c += 4
        strnum = int.from_bytes(strnum, byteorder='little', signed=True)
        s = self.strtab[strnum]
        self.s.append(s)
        if self.debug: print(f'ldstr {s}')

    def run_ldsym(self):
        symnum = self.code[self.c:self.c+4]
        self.c += 4
        symnum = int.from_bytes(symnum, byteorder='little', signed=True)
        try:
            s = self.symtab[symnum]
        except IndexError:
            raise RunError(f'Invalid symbol index: {symnum} (symtab size: {len(self.symtab)})')
        self.s.append(s)
        if self.debug: print(f'ldsym {s}')

    def run_ld(self):
        frame_idx = self.code[self.c:self.c+2]
        index = self.code[self.c+2:self.c+4]
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

        self.s.append(value)
        if self.debug: print(f'ld [{frame_idx}, {index}] -- frame={frame} value={value}')

    def run_st(self):
        frame_idx = self.code[self.c:self.c+2]
        index = self.code[self.c+2:self.c+4]
        self.c += 4

        frame_idx = int.from_bytes(frame_idx, byteorder='little', signed=False)
        index = int.from_bytes(index, byteorder='little', signed=False)

        value = self.s.pop()
        self.e[frame_idx][index] = value
        if self.debug: print(f'st {value} => [{frame_idx}, {index}]')

    def run_sel(self):
        true_len = self.code[self.c:self.c+4]
        true_len = int.from_bytes(true_len, byteorder='little', signed=False)
        self.c += 4

        false_len = self.code[self.c:self.c+4]
        false_len = int.from_bytes(false_len, byteorder='little', signed=False)
        self.c += 4

        cond = self.s.pop()
        self.d.append(self.create_continuation(offset=true_len + false_len))
        if cond == Bool(False):
            self.c += true_len

        if self.debug: print(f'sel cond={cond}')

    def run_join(self):
        self.resume_continuation(self.d.pop(), retvals=[self.s[-1]])
        if self.debug: print(f'join')

    def run_ldf(self):
        nparams = self.code[self.c:self.c+4]
        body_size = self.code[self.c+4:self.c+8]
        nparams = int.from_bytes(nparams, byteorder='little', signed=True)
        body_size = int.from_bytes(body_size, byteorder='little', signed=False)
        self.c += 8
        if nparams < 0:
            rest_param = True
            nparams = -nparams - 1
        else:
            rest_param = False
        closure = Closure(self.c, self.e, nparams=nparams, rest_param=rest_param)
        self.s.append(closure)
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

        closure = self.s.pop()
        args = self.s.pop()

        if not isinstance(closure, Closure):
            raise RunError(f'Cannot call non-function value: {closure}')
        if not isinstance(args, List):
            raise RunError(f'Invalid argument list: {args}')
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

        if self.debug: print(f'{name} {self.c} => {closure.c}')

        args = self.fit_args(closure, args)
        if isinstance(closure, Continuation):
            if len(args) != 1:
                raise RunError(
                    'Continuation should be passed one, and only one, '
                    'argument.')
            self.s = closure.s + [args[0]]
            self.e = closure.e
            self.c = closure.c
            self.d = closure.d
        else:
            if dummy_frame:
                # we've already placed the arguments inside closure.e, so we
                # won't need to add the arguments to e here
                self.s, self.e, self.c = [], closure.e, closure.c
            else:
                self.s, self.e, self.c = [], [args] + closure.e, closure.c

    def run_ap(self):
        self._do_apply('ap', tail_call=False)

    def run_ret(self):
        retval = self.s.pop()
        self.resume_continuation(self.d.pop(), [retval])
        if self.debug: print(f'ret retval={retval}')

    def run_print(self):
        v = self.s.pop()
        if self.debug: print(f'print {v}')
        print_value(v)
        self.s.append(v)  # return nil

    def run_printc(self):
        n = self.s.pop()
        if self.debug: print(f'printc {n}')
        print(chr(n), end='')
        self.s.append([])  # return nil

    def run_halt(self):
        self.halt_code = self.s.pop()
        if not isinstance(self.halt_code, int):
            raise RunError(f'Non-numeric halt code: {self.halt_code}')
        self.s.append([])  # return nil
        if self.debug: print(f'halt {self.halt_code}')

    def run_iadd(self):
        arg1 = self.s.pop()
        arg2 = self.s.pop()
        if not isinstance(arg1, int) or not isinstance(arg2, int):
            raise RunError('"iadd" arguments must be integers.')
        self.s.append(arg2 + arg1)
        if self.debug: print(f'iadd {arg2} + {arg1}')

    def run_isub(self):
        arg1 = self.s.pop()
        arg2 = self.s.pop()
        if not isinstance(arg1, int) or not isinstance(arg2, int):
            raise RunError('"isub" arguments must be integers.')
        self.s.append(arg1 - arg2)
        if self.debug: print(f'isub {arg1} - {arg2}')

    def run_imul(self):
        arg1 = self.s.pop()
        arg2 = self.s.pop()
        if not isinstance(arg1, int) or not isinstance(arg2, int):
            raise RunError('"imul" arguments must be integers.')
        self.s.append(arg1 * arg2)
        if self.debug: print(f'imul {arg2} * {arg1}')

    def run_idiv(self):
        a = self.s.pop()
        b = self.s.pop()
        if not isinstance(b, int) or not isinstance(a, int):
            raise RunError('"idiv" arguments must be integers.')
        if b == 0:
            raise RunError('Division by zero')
        self.s.append(a // b)
        if self.debug: print(f'idiv {a} / {b}')

    def run_irem(self):
        a = self.s.pop()
        b = self.s.pop()
        if not isinstance(b, int) or not isinstance(a, int):
            raise RunError('"irem" arguments must be integers.')
        if b == 0:
            raise RunError('Division by zero')

        result = abs(a) % abs(b) * (1,-1)[a < 0]

        self.s.append(result)
        if self.debug: print(f'irem {a} % {b}')

    def run_shr(self):
        n = self.s.pop()
        shift = self.s.pop()
        if not isinstance(n, int) or not isinstance(shift, int):
            raise RunError('"shr" arguments must be integers.')
        self.s.append((n % 0x100000000) >> shift)  # assuming 32 bits
        if self.debug: print(f'shr {n} >> {shift}')

    def run_shl(self):
        n = self.s.pop()
        shift = self.s.pop()
        if not isinstance(n, int) or not isinstance(shift, int):
            raise RunError('"shl" arguments must be integers.')
        self.s.append(n << shift)
        if self.debug: print(f'shl {n} << {shift}')

    def run_asr(self):
        n = self.s.pop()
        shift = self.s.pop()
        if not isinstance(n, int) or not isinstance(shift, int):
            raise RunError('"shl" arguments must be integers.')
        self.s.append(n >> shift)
        if self.debug: print(f'asr {n} >> {shift}')

    def run_bnot(self):
        n = self.s.pop()
        if not isinstance(n, int):
            raise RunError('"bnot" argument must be an integer.')
        self.s.append(~n)
        if self.debug: print(f'bnot {n} => {~n}')

    def run_band(self):
        n1 = self.s.pop()
        n2 = self.s.pop()
        if not isinstance(n1, int) or not isinstance(n2, int):
            raise RunError('"band" arguments must be integers.')
        self.s.append(n1 & n2)
        if self.debug: print(f'asr {n1} & {n2}')

    def run_bor(self):
        n1 = self.s.pop()
        n2 = self.s.pop()
        if not isinstance(n1, int) or not isinstance(n2, int):
            raise RunError('"bor" arguments must be integers.')
        self.s.append(n1 | n2)
        if self.debug: print(f'bor {n1} | {n2}')

    def run_bxor(self):
        n1 = self.s.pop()
        n2 = self.s.pop()
        if not isinstance(n1, int) or not isinstance(n2, int):
            raise RunError('"bxor" arguments must be integers.')
        self.s.append(n1 ^ n2)
        if self.debug: print(f'bxor {n1} | {n2}')

    def run_ilt(self):
        arg1 = self.s.pop()
        arg2 = self.s.pop()
        if not isinstance(arg1, int) or not isinstance(arg2, int):
            raise RunError('"lt" arguments must be integers.')
        self.s.append(Bool(True) if arg1 < arg2 else Bool(False))
        if self.debug: print(f'ilt {arg1} < {arg2}')

    def run_ile(self):
        arg1 = self.s.pop()
        arg2 = self.s.pop()
        if not isinstance(arg1, int) or not isinstance(arg2, int):
            raise RunError('"le" arguments must be integers.')
        self.s.append(Bool(True) if arg1 <= arg2 else Bool(False))
        if self.debug: print(f'ile {arg1} <= {arg2}')

    def run_dum(self):
        self.e = [self.dummy_frame] + self.e
        if self.debug: print(f'dum')

    def run_rap(self):
        self._do_apply('rap', dummy_frame=True)

    def run_tap(self):
        self._do_apply('tap', tail_call=True)

    def run_drop(self):
        value = self.s.pop()
        if self.debug: print(f'drop {value}')

    def run_xp(self):
        self.e[0].append([])  # expand frame by adding a nil value to it
        if self.debug: print(f'xp')

    def run_dup(self):
        self.s.append(self.s[-1])
        if self.debug: print(f'dup {self.s[-1]}')

    def run_true(self):
        self.s.append(Bool(True))
        if self.debug: print(f'true')

    def run_false(self):
        self.s.append(Bool(False))
        if self.debug: print(f'false')

    def run_strtab(self):
        nstrs = self.code[self.c:self.c+4]
        self.c += 4
        nstrs = int.from_bytes(nstrs, byteorder='little', signed=False)
        self.strtab = [String('')]
        for _ in range(nstrs):
            length = self.code[self.c:self.c+4]
            self.c += 4
            length = int.from_bytes(length, byteorder='little', signed=False)
            s = self.code[self.c:self.c+length]
            s = String.from_bytes(s)
            self.strtab.append(s)
            self.c += length

        if self.debug: print(f'strtab {nstrs}')

    def run_symtab(self):
        if self.symtab != []:
            raise RunError('Multiple symtab instructions')
        nsyms = self.code[self.c:self.c+4]
        self.c += 4
        nsyms = int.from_bytes(nsyms, byteorder='little', signed=False)
        strnums = []
        for _ in range(nsyms):
            strnum = self.code[self.c:self.c+4]
            self.c += 4
            strnum = int.from_bytes(strnum, byteorder='little', signed=False)
            strnums.append(strnum)

        self.symtab = [Symbol(self.strtab[i].value) for i in strnums]
        if self.debug: print(f'symtab {nsyms}')

    def run_car(self):
        pair = self.s.pop()

        if not isinstance(pair, Pair):
            raise RunError(f'Not a pair to get car of: {pair}')

        car = pair.car
        self.s.append(car)
        if self.debug: print(f'car => {car}')

    def run_cdr(self):
        pair = self.s.pop()

        if not isinstance(pair, Pair):
            raise RunError(f'Not a pair to get cdr of: {pair}')

        cdr = pair.cdr
        self.s.append(cdr)
        if self.debug: print(f'cdr => {cdr}')

    def run_type(self):
        v = self.s.pop()
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
        self.s.append(result)
        if self.debug: print(f'type {type(v)} => {result}')

    def run_eq(self):
        x = self.s.pop()
        y = self.s.pop()
        result = Bool(x == y)
        self.s.append(result)
        if self.debug: print(f'eq {result}')

    def run_set(self):
        symnum = self.code[self.c:self.c+4]
        self.c += 4
        symnum = int.from_bytes(symnum, byteorder='little', signed=True)

        value = self.s.pop()
        self.symvals[symnum] = value

        if self.debug: print(f'set {self.symtab[symnum]} => {value}')

    def run_get(self):
        symnum = self.code[self.c:self.c+4]
        self.c += 4
        symnum = int.from_bytes(symnum, byteorder='little', signed=True)

        try:
            # leave the set value on the stack as the return value of set
            value = self.symvals[symnum]
        except KeyError:
            if 0 <= symnum < len(self.symtab):
                sym = self.symtab[symnum]
                raise RunError(f'Attempt to read unset symbol: {sym} ({symnum})')
            else:
                raise RunError(f'Unknown symbol number set: {symnum}')

        self.s.append(value)
        if self.debug: print(f'get {self.symtab[symnum]} => {value}')

    def run_error(self):
        if self.debug: print(f'error')
        raise UserError('User error')

    def run_gensym(self):
        sym = Symbol.gensym()
        self.s.append(sym)
        if self.debug: print(f'gensym {sym}')

    def run_ccc(self): # call/cc
        closure = self.s.pop()
        if not isinstance(closure, Closure):
            raise RunError(f'ccc with non-function value: {closure}')

        cont = self.create_continuation()
        args = List.from_list([cont])
        self.d.append(cont)

        self.s.append(args)
        self.s.append(closure)
        self._do_apply('ccc')

    def run_i2ch(self):
        char_code = self.s.pop()
        if not isinstance(char_code, int):
            raise RunError(f'Invalid character code for i2ch: {char_code}')
        self.s.append(Char(char_code))
        if self.debug: print(f'i2ch {char_code}')

    def run_ch2i(self):
        char = self.s.pop()
        if not isinstance(char, Char):
            raise RunError(f'ch2i argument not a character: {char}')
        self.s.append(char.char_code)
        if self.debug: print(f'ch2i {char}')

    def run_ugcat(self):  # unicode general category
        char = self.s.pop()
        if not isinstance(char, Char):
            raise RunError(f'ugcat argument not a character: {char}')
        cat = unicodedata.category(chr(char.char_code))
        sym = self.intern(cat)
        self.s.append(sym)
        if self.debug: print(f'ugcat {char}')

    def run_chup(self):
        char = self.s.pop()
        if not isinstance(char, Char):
            raise RunError(f'chup argument not a character: {char}')
        new_char = Char(ord(chr(char.char_code).upper()))
        self.s.append(new_char)
        if self.debug: print(f'chup {char}')

    def run_chdn(self):
        char = self.s.pop()
        if not isinstance(char, Char):
            raise RunError(f'chdn argument not a character: {char}')
        new_char = Char(ord(chr(char.char_code).lower()))
        self.s.append(new_char)
        if self.debug: print(f'chdn {char}')

    def run_chfd(self):
        char = self.s.pop()
        if not isinstance(char, Char):
            raise RunError(f'chfd argument not a character: {char}')
        folded_char = chr(char.char_code).casefold()
        if len(folded_char) > 1:
            # for example: german letter ß becomes "ss" when folded. not sure
            # what else to do besides defaulting to lower case. chez scheme also
            # seems to be doing the same thing.
            folded_char = chr(char.char_code).lower()
        new_char = Char(ord(folded_char))
        self.s.append(new_char)
        if self.debug: print(f'chfd {char}')

    def run_chdv(self):
        char = self.s.pop()
        if not isinstance(char, Char):
            raise RunError(f'chdv argument not a character: {char}')
        digit_value = unicodedata.digit(chr(char.char_code), None)
        if digit_value is None:
            digit_value = Bool(False)
        self.s.append(digit_value)
        if self.debug: print(f'chdv {char}')

    def run_mkstr(self):
        nchars = self.s.pop()
        fill_char = self.s.pop()

        if not isinstance(nchars, int):
            raise RunError(f'mkstr argument "nchars" not an int: {nchars}')

        if not isinstance(fill_char, Char):
            raise RunError(f'mkstr argument "fill_char" not a char: {fill_char}')

        s = chr(fill_char.char_code) * nchars
        s = String(s)
        self.s.append(s)
        if self.debug: print(f'mkstr {nchars} * {fill_char}')

    def run_strref(self):
        s = self.s.pop()
        idx = self.s.pop()

        if not isinstance(idx, int):
            raise RunError(f'strref argument "index" not an int: {idx}')

        if not isinstance(s, String):
            raise RunError(f'strref argument "string" not a string: {s}')

        try:
            c = s.value[idx]
        except IndexError:
            raise RunError(f'Invalid index {idx} for string {s}')

        c = Char(ord(c))
        self.s.append(c)
        if self.debug: print(f'strref: s={s} idx={idx} => {c}')

    def run_strset(self):
        s = self.s.pop()
        idx = self.s.pop()
        char = self.s.pop()

        if not isinstance(char, Char):
            raise RunError(f'strset argument "char" not a char: {char}')

        if not isinstance(idx, int):
            raise RunError(f'strset argument "index" not an int: {idx}')

        if not isinstance(s, String):
            raise RunError(f'strset argument "string" not a string: {s}')

        if idx < 0 or idx >= len(s):
            raise RunError(f'Invalid index {idx} for string {s}')

        s.value = s.value[:idx] + chr(char.char_code) + s.value[idx + 1:]

        self.s.append(s)
        if self.debug: print(f'strset: idx={idx} char={char} => {s}')

    def run_strlen(self):
        s = self.s.pop()
        if not isinstance(s, String):
            raise RunError(f'strlen argument not a string: {s}')
        self.s.append(len(s))
        if self.debug: print(f'strlen {s} => {len(s)}')

    def run_setcar(self):
        pair = self.s.pop()
        value = self.s.pop()

        if not isinstance(pair, Pair):
            raise RunError(f'Not a pair to set-car: {pair}')

        pair.car = value
        self.s.append(pair)
        if self.debug: print(f'setcar pair={pair} value={value}')

    def run_setcdr(self):
        pair = self.s.pop()
        value = self.s.pop()

        if not isinstance(pair, Pair):
            raise RunError(f'Not a pair to set-cdr: {pair}')

        pair.cdr = value
        self.s.append(pair)
        if self.debug: print(f'setcdr pair={pair} value={value}')


def configure_argparse(parser: argparse.ArgumentParser):
    parser.description = 'Run a binary SECD program'

    parser.add_argument(
        'input', default='-', nargs='?',
        help='Input file. Stdin is used if not specified or a dash (-) '
        'is passed instead. Defaults to reading from stdin.')

    parser.add_argument(
        '--debug', '-g', action='store_true', default=False,
        help='Enable debug mode.')

    parser.set_defaults(func=main)


def main(args):
    if args.input == '-':
        code = sys.stdin.buffer.read()
    else:
        with open(args.input, 'rb') as f:
            code = f.read()

    m = Secd(code)
    m.debug = args.debug
    try:
        m.run()
    except UserError:
        err = m.s[-1]
        msg = format_user_error(err)
        print(f'Run error: {msg}', file=sys.stderr)
        sys.exit(1)
    except RunError as e:
        print('Run error:', e)

    if m.halt_code is None:
        print('Code exhausted.', file=sys.stderr)
    else:
        print('Machine halted with code:', m.halt_code, file=sys.stderr)


def print_value(v):
    if isinstance(v, String):
        print(v.value)
    elif isinstance(v, (int, Symbol, Bool, List, Closure, Char)):
        print(v)
    else:
        raise RunError(f'Unknown type to print: {v}')
