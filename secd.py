#!/usr/bin/env python3

import argparse
import sys
import sexpr


dummy_frame = object()


class CompileError(Exception):
    pass


class RunError(Exception):
    pass


class Secd:
    def __init__(self, code):
        self.s = []
        self.e = []
        self.c = code
        self.d = []
        self.ip = 0
        self.halt_code = False


    def run(self):
        self.halt_code = None
        self.ip = 0
        while self.ip < len(self.c) and self.halt_code is None:
            instr = self.c[self.ip]
            self.ip += 1
            instr.execute(self)


    def halt(self, halt_code):
        self.halt_code = halt_code


class Instr:
    def __repr__(self):
        return f'{self.__class__.__name__.lower()}'


class Nil(Instr):
    def execute(self, machine):
        machine.s.append([])


class Cons(Instr):
    def execute(self, machine):
        v = machine.s.pop()
        l = machine.s.pop()
        machine.s.append(l + [v])


class Ldc(Instr):
    def __init__(self, value):
        self.value = value

    def execute(self, machine):
        machine.s.append(self.value)

    def __repr__(self):
        return f'ldc {self.value}'


class Ld(Instr):
    def __init__(self, frame, index ):
        self.frame = frame
        self.index = index

    def execute(self, machine):
        frame = machine.e[self.frame]
        value = frame[self.index]
        machine.s.append(value)

    def __repr__(self):
        return f'ld ({self.frame} {self.index})'


class Sel(Instr):
    def __init__(self, true_body, false_body):
        self.true_body = true_body
        self.false_body = false_body

    def execute(self, machine):
        cond = machine.s.pop()
        machine.d.append(machine.c[machine.ip:])
        if cond != 0:
            machine.c = self.true_body
        else:
            machine.c = self.false_body
        machine.ip = 0

    def __repr__(self):
        true_body = ' '.join(repr(i) for i in self.true_body)
        false_body = ' '.join(repr(i) for i in self.false_body)
        return f'sel ({true_body}) ({false_body})'


class Join(Instr):
    def execute(self, machine):
        machine.c = machine.d.pop()
        machine.ip = 0


class Ldf(Instr):
    def __init__(self, body):
        self.body = body

    def execute(self, machine):
        closure = (self.body, machine.e)
        machine.s.append(closure)

    def __repr__(self):
        body = ' '.join(repr(i) for i in self.body)
        return f'ldf ({body})'


class Ap(Instr):
    def execute(self, machine):
        closure = machine.s.pop()
        args = machine.s.pop()
        machine.d.append((machine.s, machine.e, machine.c[machine.ip:]))
        new_code, new_env = closure
        machine.c, machine.e = new_code, [args] + new_env
        machine.s = []
        machine.ip = 0


class Ret(Instr):
    def execute(self, machine):
        if len(machine.s) > 0:
            retval = machine.s.pop()
        else:
            retval = [] # nil
        machine.s, machine.e, machine.c = machine.d.pop()
        machine.s.append(retval)
        machine.ip = 0


class Printn(Instr):
    def execute(self, machine):
        n = machine.s.pop()
        print(n)


class Printc(Instr):
    def execute(self, machine):
        n = machine.s.pop()
        print(chr(n), end='')


class Halt(Instr):
    def execute(self, machine):
        exit_code = machine.s.pop()
        if not isinstance(exit_code, int):
            raise RunError(f'Non-numeric exit code: {exit_code}')
        machine.halt(exit_code)


class Add(Instr):
    def execute(self, machine):
        arg1 = machine.s.pop()
        arg2 = machine.s.pop()
        if not isinstance(arg1, int) or not isinstance(arg2, int):
            raise RunError('"add" arguments must be integers.')
        machine.s.append(arg2 + arg1)


class Sub(Instr):
    def execute(self, machine):
        arg1 = machine.s.pop()
        arg2 = machine.s.pop()
        if not isinstance(arg1, int) or not isinstance(arg2, int):
            raise RunError('"sub" arguments must be integers.')
        machine.s.append(arg2 - arg1)


class Lt(Instr):
    def execute(self, machine):
        arg1 = machine.s.pop()
        arg2 = machine.s.pop()
        if not isinstance(arg1, int) or not isinstance(arg2, int):
            raise RunError('"lt" arguments must be integers.')
        machine.s.append(1 if arg2 < arg1 else 0)


class Dum(Instr):
    def execute(self, machine):
        machine.e = [dummy_frame] + machine.e


class Rap(Instr):
    def execute(self, machine):
        closure = machine.s.pop()
        args = machine.s.pop()
        machine.d.append((machine.s, machine.e, machine.c[machine.ip:]))
        new_code, new_env = closure
        if new_env[0] != dummy_frame:
            raise RunError('No dummy frame.')

        # replace dummy frame with actual frame
        new_env[0] = args

        machine.s, machine.e, machine.c = [], new_env, new_code
        machine.ip = 0


def compile(input):
    code = []
    i = 0
    while i < len(input):
        instr = input[i]
        if not isinstance(instr, sexpr.Symbol):
            raise CompileError(f'Invalid SECD instruction: {instr}')

        instr = instr.name

        no_arg_instrs = {
            'nil': Nil,
            'cons': Cons,
            'join': Join,
            'ap': Ap,
            'ret': Ret,
            'dum': Dum,
            'rap': Rap,
            'printn': Printn,
            'printc': Printc,
            'halt': Halt,
            'add': Add,
            'sub': Sub,
            'lt': Lt,
        }
        if instr in no_arg_instrs:
            klass = no_arg_instrs[instr]
            code.append(klass())
        elif instr == 'ldc':
            i += 1
            if i >= len(input):
                raise CompileError('Missing ldc argument')
            value = input[i]
            code.append(Ldc(value))
        elif instr == 'ld':
            i += 1
            if i >= len(input):
                raise CompileError('Missing ld argument')
            value = input[i]
            if not isinstance(value, list) or len(value) != 2:
                raise CompileError('ld expects a list of size 2.')
            frame, index = value
            code.append(Ld(frame, index))
        elif instr == 'sel':
            i += 1
            if i >= len(input):
                raise CompileError('Missing sel arguments')
            value1 = input[i]
            i += 1
            if i >= len(input):
                raise CompileError('Mising sel argument')
            value2 = input[i]
            if not isinstance(value1, list) or not isinstance(value2, list):
                raise CompileError('SEL expects two lists')
            value1 = compile(value1)
            value2 = compile(value2)
            code.append(Sel(value1, value2))
        elif instr == 'ldf':
            i += 1
            if i >= len(input):
                raise CompileError('Missing ldf argument')
            value = input[i]
            if not isinstance(value, list):
                raise CompileError('ldf expects a list')
            value = compile(value)
            code.append(Ldf(value))
        else:
            raise CompileError(f'Unknown instruction: {instr}')
        i += 1

    return code


def main():
    parser = argparse.ArgumentParser(
        description='Run SECD instructions written in sexpr form.')

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
        expr = sexpr.read(text)
    except sexpr.ParseError as e:
        print(f'Parse error: {e}')
        sys.exit(1)

    try:
        code = compile(expr)
    except CompileError as e:
        print(f'Compile error: {e}')
        sys.exit(1)

    m = Secd(code)
    m.run()
    exit_code = m.halt_code
    if exit_code is None:
        print('Code exhausted.')
    else:
        print('Machine halted with code:', exit_code)


if __name__ == '__main__':
    main()
