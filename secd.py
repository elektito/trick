#!/usr/bin/env python3

import sys
import argparse
from machinetypes import Bool, Symbol
from read import print_sexpr


class RunError(Exception):
    pass


class Closure:
    def __init__(self, c, e):
        self.c = c
        self.e = e

    def __repr__(self):
        return f'<Closure c={self.c} e={self.e}>'


class Secd:
    def __init__(self, code):
        self.code = code

        self.s = []
        self.e = [[]]
        self.c = 0
        self.d = []
        self.halt_code = None
        self.dummy_frame = object()
        self.debug = False

    def run(self):
        funcs = {
            0x01: self.run_nil,
            0x02: self.run_cons,
            0x03: self.run_join,
            0x04: self.run_ap,
            0x05: self.run_ret,
            0x06: self.run_print,
            0x07: self.run_printc,
            0x08: self.run_halt,
            0x09: self.run_add,
            0x0a: self.run_sub,
            0x0b: self.run_lt,
            0x0c: self.run_dum,
            0x0d: self.run_rap,
            0x0e: self.run_tap,
            0x0f: self.run_drop,
            0x10: self.run_xp,
            0x11: self.run_dup,
            0x12: self.run_true,
            0x13: self.run_false,
            0x20: self.run_ldc,
            0x21: self.run_ld,
            0x22: self.run_sel,
            0x23: self.run_ldf,
            0x24: self.run_st,
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

    def run_nil(self):
        self.s.append([])
        if self.debug: print('nil')

    def run_cons(self):
        v = self.s.pop()
        l = self.s.pop()
        self.s.append(l + [v])
        if self.debug: print(f'cons {v} onto {l}')

    def run_ldc(self):
        value = self.code[self.c:self.c+4]
        self.c += 4
        value = int.from_bytes(value, byteorder='little', signed=True)
        self.s.append(value)
        if self.debug: print(f'ldc {value}')

    def run_ld(self):
        frame_idx = self.code[self.c:self.c+2]
        index = self.code[self.c+2:self.c+4]
        self.c += 4

        frame_idx = int.from_bytes(frame_idx, byteorder='little', signed=False)
        index = int.from_bytes(index, byteorder='little', signed=False)

        frame = self.e[frame_idx]
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
        if not isinstance(cond, Bool):
            raise RunError(f'Condition value is not a boolean: {cond}')
        self.d.append(self.c + true_len + false_len)
        if not cond:
            self.c += true_len

        if self.debug: print(f'sel cond={cond}')

    def run_join(self):
        self.c = self.d.pop()
        if self.debug: print(f'join')

    def run_ldf(self):
        body_size = self.code[self.c:self.c+4]
        body_size = int.from_bytes(body_size, byteorder='little', signed=False)
        self.c += 4
        closure = Closure(self.c, self.e)
        self.s.append(closure)
        self.c += body_size
        if self.debug: print(f'ldf body_size={body_size}')

    def run_ap(self):
        closure = self.s.pop()
        args = self.s.pop()
        self.d.append((self.s, self.e, self.c))
        if self.debug: print(f'ap {self.c} => {closure.c}')
        self.s, self.e, self.c = [], [args] + closure.e, closure.c

    def run_ret(self):
        retval = self.s.pop()
        self.s, self.e, self.c = self.d.pop()
        self.s.append(retval)
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

    def run_add(self):
        arg1 = self.s.pop()
        arg2 = self.s.pop()
        if not isinstance(arg1, int) or not isinstance(arg2, int):
            raise RunError('"add" arguments must be integers.')
        self.s.append(arg2 + arg1)
        if self.debug: print(f'add {arg2} + {arg1}')

    def run_sub(self):
        arg1 = self.s.pop()
        arg2 = self.s.pop()
        if not isinstance(arg1, int) or not isinstance(arg2, int):
            raise RunError('"lt" arguments must be integers.')
        self.s.append(arg2 - arg1)
        if self.debug: print(f'sub {arg2} - {arg1}')

    def run_lt(self):
        arg1 = self.s.pop()
        arg2 = self.s.pop()
        if not isinstance(arg1, int) or not isinstance(arg2, int):
            raise RunError('"lt" arguments must be integers.')
        self.s.append(Bool(True) if arg2 < arg1 else Bool(False))
        if self.debug: print(f'lt {arg2} < {arg1}')

    def run_dum(self):
        self.e = [self.dummy_frame] + self.e
        if self.debug: print(f'dum')

    def run_rap(self):
        closure = self.s.pop()
        args = self.s.pop()

        if closure.e[0] != self.dummy_frame:
            raise RunError('No dummy frame.')

        # note that we don't store e[0] on d, since it contains the dummy frame.
        # in normal 'ap' that does not exist, so we can store the entire
        # contents of e.
        self.d.append((self.s, self.e[1:], self.c))

        # replace dummy frame with actual frame
        closure.e[0] = args

        if self.debug: print(f'rap {self.c} => {closure.c}')
        self.s, self.e, self.c = [], closure.e, closure.c

    def run_tap(self):
        closure = self.s.pop()
        args = self.s.pop()
        if self.debug: print(f'tap {self.c} => {closure.c}')
        self.s, self.e, self.c = [], [args] + closure.e, closure.c

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


def main():
    parser = argparse.ArgumentParser(
        description='Run binary SECD instructions.')

    parser.add_argument(
        'input', default='-', nargs='?',
        help='Input file. Stdin is used if not specified or a dash (-) '
        'is passed instead. Defaults to reading from stdin.')

    args = parser.parse_args()

    if args.input == '-':
        code = sys.stdin.buffer.read()
    else:
        with open(args.input, 'rb') as f:
            code = f.read()

    m = Secd(code)
    # m.debug = True
    try:
        m.run()
    except RunError as e:
        print(f'Run error: {e}', file=sys.stderr)
        sys.exit(1)

    if m.halt_code is None:
        print('Code exhausted.', file=sys.stderr)
    else:
        print('Machine halted with code:', m.halt_code, file=sys.stderr)


def print_value(v):
    if isinstance(v, Symbol):
        print(v.name)
    elif isinstance(v, int):
        print(v)
    elif isinstance(v, Bool):
        print('#t' if v else '#f')
    elif isinstance(v, list):
        print_sexpr(v)
    elif isinstance(v, Closure):
        print(v)
    else:
        raise RunError(f'Unknown type to print: {v}')


if __name__ == '__main__':
    main()
