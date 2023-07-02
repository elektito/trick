#!/usr/bin/env python3

import sys


class Dummy:
    pass
dummy_frame = Dummy()


def error(msg):
    print(msg, file=sys.stderr)
    sys.exit(1)


pending = ''
def getc():
    global pending
    if pending:
        c, pending = pending[:1], pending[1:]
        return c
    return sys.stdin.read(1)


def ungetc(c):
    global pending
    pending += c


def read_token():
    while True:
        c = getc()
        if c == '':
            return ''
        if not c.isspace():
            break

    if c in '()':
        return c

    tok = ''
    while True:
        if c == '':
            return tok
        if c in '()' or c.isspace():
            ungetc(c)
            return tok
        tok += c
        c = getc()


def read_list():
    items = []
    while True:
        value = read()
        if value == '':
            error('EOF while reading list.')
        if value == ')':
            return items
        items.append(value)


def read():
    tok = read_token()
    if tok == '':
        error('EOF while parsing input.')
    if tok == '(':
        return read_list()
    if tok == ')':
        return tok
    try:
        return int(tok)
    except ValueError:
        return tok


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
            error(f'Non-numeric exit code: {exit_code}')
        machine.halt(exit_code)


class Add(Instr):
    def execute(self, machine):
        arg1 = machine.s.pop()
        arg2 = machine.s.pop()
        if not isinstance(arg1, int) or not isinstance(arg2, int):
            error('+ arguments must be integers.')
        machine.s.append(arg2 + arg1)


class Sub(Instr):
    def execute(self, machine):
        arg1 = machine.s.pop()
        arg2 = machine.s.pop()
        if not isinstance(arg1, int) or not isinstance(arg2, int):
            error('+ arguments must be integers.')
        machine.s.append(arg2 - arg1)


class Lt(Instr):
    def execute(self, machine):
        arg1 = machine.s.pop()
        arg2 = machine.s.pop()
        if not isinstance(arg1, int) or not isinstance(arg2, int):
            error('+ arguments must be integers.')
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
            error('No dummy frame.')

        # replace dummy frame with actual frame
        new_env[0] = args

        machine.s, machine.e, machine.c = [], new_env, new_code
        machine.ip = 0


def compile(sexpr):
    code = []
    i = 0
    while i < len(sexpr):
        instr = sexpr[i]
        if instr == 'nil':
            code.append(Nil())
        elif instr == 'cons':
            code.append(Cons())
        elif instr == 'ldc':
            i += 1
            if i >= len(sexpr): error('Unexpected eof when compiling.')
            value = sexpr[i]
            code.append(Ldc(value))
        elif instr == 'ld':
            i += 1
            if i >= len(sexpr): error('Unexpected eof when compiling.')
            value = sexpr[i]
            if not isinstance(value, list) or len(value) != 2:
                error('ld expects a list of size 2.')
            frame, index = value
            code.append(Ld(frame, index))
        elif instr == 'sel':
            i += 1
            if i >= len(sexpr): error('Unexpected eof when compiling.')
            value1 = sexpr[i]
            i += 1
            if i >= len(sexpr): error('Unexpected eof when compiling.')
            value2 = sexpr[i]
            if not isinstance(value1, list) or not isinstance(value2, list):
                error('SEL expects two lists')
            value1 = compile(value1)
            value2 = compile(value2)
            code.append(Sel(value1, value2))
        elif instr == 'join':
            code.append(Join())
        elif instr == 'ldf':
            i += 1
            if i >= len(sexpr): error('Unexpected eof when compiling.')
            value = sexpr[i]
            if not isinstance(value, list):
                error('ldf expects a list.')
            value = compile(value)
            code.append(Ldf(value))
        elif instr == 'ap':
            code.append(Ap())
        elif instr == 'ret':
            code.append(Ret())
        elif instr == 'dum':
            code.append(Dum())
        elif instr == 'rap':
            code.append(Rap())
        elif instr == 'printn':
            code.append(Printn())
        elif instr == 'printc':
            code.append(Printc())
        elif instr == 'halt':
            code.append(Halt())
        elif instr == 'add':
            code.append(Add())
        elif instr == 'sub':
            code.append(Sub())
        elif instr == 'lt':
            code.append(Lt())
        else:
            error(f'Unknown instruction when compiling: {instr}')
        i += 1

    return code


def compile_lisp_int(expr, env):
    return ['ldc', expr]


def compile_lisp_printn(expr, env):
    if len(expr) != 2:
        error(f'Invalid number of arguments for printn: {expr}')
    code = compile_lisp(expr[1], env)
    return code + ['printn']


def compile_lisp_printc(expr, env):
    if len(expr) != 2:
        error(f'Invalid number of arguments for printc: {expr}')
    code = compile_lisp(expr[1], env)
    return code + ['printc']


def compile_lisp_halt(expr, env):
    if len(expr) != 2:
        error(f'Invalid number of arguments for halt: {expr}')
    code = compile_lisp(expr[1], env)
    return code + ['halt']


def compile_lisp_if(expr, env):
    if len(expr) != 4:
        error(f'Invalid number of arguments for if: {expr}')

    cond_code = compile_lisp(expr[1], env)
    true_code = compile_lisp(expr[2], env) + ['join']
    false_code = compile_lisp(expr[3], env) + ['join']
    return cond_code + ['sel'] + [true_code] + [false_code]


def compile_lisp_add(expr, env):
    if len(expr) != 3:
        error(f'Invalid number of arguments for +: {expr}')

    arg1 = compile_lisp(expr[1], env)
    arg2 = compile_lisp(expr[2], env)
    return arg1 + arg2 + ['add']


def compile_lisp_sub(expr, env):
    if len(expr) != 3:
        error(f'Invalid number of arguments for -: {expr}')

    arg1 = compile_lisp(expr[1], env)
    arg2 = compile_lisp(expr[2], env)
    return arg1 + arg2 + ['sub']


def compile_lisp_lt(expr, env):
    if len(expr) != 3:
        error(f'Invalid number of arguments for <: {expr}')

    arg1 = compile_lisp(expr[1], env)
    arg2 = compile_lisp(expr[2], env)
    return arg1 + arg2 + ['lt']


def compile_lisp_lambda(expr, env):
    if len(expr) < 3:
        error(f'Invalid number of arguments for lambda: {expr}')

    params = expr[1]
    new_env = [params] + env

    code = []
    for e in expr[2:]:
        code += compile_lisp(e, new_env)

    return ['ldf'] + [code + ['ret']]


def compile_lisp_symbol(expr, env):
    sym = expr
    for i, frame in enumerate(env):
        if sym in frame:
            return ['ld', [i, frame.index(sym)]]
    error(f'Unknown symbol: {sym}')


def compile_lisp_let(expr, env):
    if len(expr) < 2:
        error(f'Invalid number of arguments for let: {expr}')

    if not isinstance(expr[1], list):
        error(f'Invalid bindings list for let: {expr[1]}')

    bindings = expr[1]
    for pair in bindings:
        if not isinstance(pair, list) or len(pair) != 2 or not isinstance(pair[0], str):
            error(f'Invalid let binding: {pair}')

    params = [x for x, y in bindings]
    args = [y for x, y in bindings]
    body = expr[2:]

    # transform let to a lambda call and compile that instead
    new_expr = [['lambda', params] + body] + args
    return compile_lisp(new_expr, env)


def compile_lisp_letrec(expr, env):
    if len(expr) < 2:
        error(f'Invalid number of arguments for let: {expr}')

    if not isinstance(expr[1], list):
        error(f'Invalid bindings list for let: {expr[1]}')

    bindings = expr[1]
    for pair in bindings:
        if not isinstance(pair, list) or len(pair) != 2 or not isinstance(pair[0], str):
            error(f'Invalid let binding: {pair}')

    vars = [x for x, y in bindings]
    values = [y for x, y in bindings]
    body = expr[2:]

    secd_code = ['dum', 'nil']
    for v in values:
        secd_code += compile_lisp(v, [vars] + env) + ['cons']

    lambda_form = ['lambda', vars] + body
    secd_code += compile_lisp(lambda_form, env)

    secd_code += ['rap']

    return secd_code


def compile_lisp(expr, env):
    if expr == 'nil':
        secd_code = ['nil']
    elif isinstance(expr, list) and expr[0] == 'if':
        secd_code = compile_lisp_if(expr, env)
    elif isinstance(expr, list) and expr[0] == '+':
        secd_code = compile_lisp_add(expr, env)
    elif isinstance(expr, list) and expr[0] == '-':
        secd_code = compile_lisp_sub(expr, env)
    elif isinstance(expr, list) and expr[0] == '<':
        secd_code = compile_lisp_lt(expr, env)
    elif isinstance(expr, list) and expr[0] == 'lambda':
        secd_code = compile_lisp_lambda(expr, env)
    elif isinstance(expr, list) and expr[0] == 'let':
        secd_code = compile_lisp_let(expr, env)
    elif isinstance(expr, list) and expr[0] == 'letrec':
        secd_code = compile_lisp_letrec(expr, env)
    elif isinstance(expr, list) and expr[0] == 'printn':
        secd_code = compile_lisp_printn(expr, env)
    elif isinstance(expr, list) and expr[0] == 'printc':
        secd_code = compile_lisp_printc(expr, env)
    elif isinstance(expr, list) and expr[0] == 'halt':
        secd_code = compile_lisp_halt(expr, env)
    elif isinstance(expr, int):
        secd_code = compile_lisp_int(expr, env)
    elif isinstance(expr, list):
        secd_code = ['nil']
        for arg in expr[1:]:
            secd_code += compile_lisp(arg, env)
            secd_code += ['cons']
        secd_code += compile_lisp(expr[0], env)
        secd_code += ['ap']
    else:
        secd_code = compile_lisp_symbol(expr, env)

    return secd_code


def print_sexpr(value):
    if isinstance(value, list):
        print('(', end='')
        for i in range(len(value)):
            print_sexpr(value[i])
            if i != len(value) - 1:
                print(' ', end='')
        print(')', end='')
    else:
        print(value, end='')



def compile_and_run_secd(sexpr):
    code = compile(sexpr)
    print('Code:', code)

    print('==== running ====')
    m = Secd(code)
    m.run()
    exit_code = m.halt_code
    if exit_code is None:
        print('Code exhausted.')
    else:
        print('Machine halted with code:', exit_code)
    print('==== done ====')


def main():
    sexpr = read()
    if not isinstance(sexpr, list):
        error('Input is not a list.')

    if len(sys.argv) > 1 and sys.argv[1] == 'compile':
        secd_code = compile_lisp(sexpr, [])
        print_sexpr(secd_code)
        print()
    else:
        compile_and_run_secd(sexpr)


if __name__ == '__main__':
    main()
