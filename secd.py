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


def compile(sexpr):
    code = []
    i = 0
    while i < len(sexpr):
        instr = sexpr[i]
        if instr == 'nil':
            code.append({
                'op': 'nil',
            })
        elif instr == 'cons':
            code.append({
                'op': 'cons',
            })
        elif instr == 'ldc':
            i += 1
            if i >= len(sexpr): error('Unexpected eof when compiling.')
            value = sexpr[i]
            code.append({
                'op': 'ldc',
                'value': value,
            })
        elif instr == 'ld':
            i += 1
            if i >= len(sexpr): error('Unexpected eof when compiling.')
            value = sexpr[i]
            if not isinstance(value, list) or len(value) != 2:
                error('ld expects a list of size 2.')
            code.append({
                'op': 'ld',
                'value': value,
            })
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
            code.append({
                'op': 'sel',
                'value1': value1,
                'value2': value2,
            })
        elif instr == 'join':
            code.append({
                'op': 'join',
            })
        elif instr == 'ldf':
            i += 1
            if i >= len(sexpr): error('Unexpected eof when compiling.')
            value = sexpr[i]
            if not isinstance(value, list):
                error('ldf expects a list.')
            value = compile(value)
            code.append({
                'op': 'ldf',
                'value': value,
            })
        elif instr == 'ap':
            code.append({
                'op': 'ap',
            })
        elif instr == 'ret':
            code.append({
                'op': 'ret',
            })
        elif instr == 'dum':
            code.append({
                'op': 'dum',
            })
        elif instr == 'rap':
            code.append({
                'op': 'rap',
            })
        elif instr == 'printn':
            code.append({
                'op': 'printn',
            })
        elif instr == 'printc':
            code.append({
                'op': 'printc',
            })
        elif instr == 'halt':
            code.append({
                'op': 'halt',
            })
        elif instr == 'add':
            code.append({
                'op': 'add',
            })
        elif instr == 'sub':
            code.append({
                'op': 'sub',
            })
        elif instr == 'lt':
            code.append({
                'op': 'lt',
            })
        else:
            error(f'Unknown instruction when compiling: {instr}')
        i += 1

    return code


def run(code, s=None, e=None, d=None):
    if s is None:
        s = []
    if e is None:
        e = []
    if d is None:
        d = []

    i = 0
    while i < len(code):
        #import time; time.sleep(0.2)
        instr = code[i]
        #print('iii', i, instr)
        i += 1

        if instr['op'] == 'nil':
            s.append([])
        elif instr['op'] == 'cons':
            v = s.pop()
            l = s.pop()
            s.append(l + [v])
        elif instr['op'] == 'ldc':
            s.append(instr['value'])
        elif instr['op'] == 'ld':
            f, n = instr['value']
            frame = e[f]
            value = frame[n]
            s.append(value)
        elif instr['op'] == 'sel':
            cond = s.pop()
            d.append(code[i:])
            if cond != 0:
                code = instr['value1']
            else:
                code = instr['value2']
            i = 0
        elif instr['op'] == 'join':
            code = d.pop()
            i = 0
        elif instr['op'] == 'ldf':
            closure = (instr['value'], e)
            s.append(closure)
        elif instr['op'] == 'ap':
            closure = s.pop()
            args = s.pop()
            d.append((s, e, code[i:]))
            new_code, new_env = closure
            code, e = new_code, [args] + new_env
            s = []
            i = 0
        elif instr['op'] == 'ret':
            if len(s) > 0:
                retval = s.pop()
            else:
                retval = [] # nil
            s, e, code = d.pop()
            s.append(retval)
            i = 0
        elif instr['op'] == 'printn':
            n = s.pop()
            print(n)
        elif instr['op'] == 'printc':
            c = chr(s.pop())
            print(c, end='')
        elif instr['op'] == 'halt':
            exit_code = s.pop()
            if not isinstance(exit_code, int):
                error(f'Non-numeric exit code: {exit_code}')
            return exit_code
        elif instr['op'] == 'add':
            arg1 = s.pop()
            arg2 = s.pop()
            if not isinstance(arg1, int) or not isinstance(arg2, int):
                error('+ arguments must be integers.')
            s.append(arg2 + arg1)
        elif instr['op'] == 'sub':
            arg1 = s.pop()
            arg2 = s.pop()
            if not isinstance(arg1, int) or not isinstance(arg2, int):
                error('+ arguments must be integers.')
            s.append(arg2 - arg1)
        elif instr['op'] == 'lt':
            arg1 = s.pop()
            arg2 = s.pop()
            if not isinstance(arg1, int) or not isinstance(arg2, int):
                error('+ arguments must be integers.')
            s.append(1 if arg2 < arg1 else 0)
        elif instr['op'] == 'dum':
            e = [dummy_frame] + e
        elif instr['op'] == 'rap':
            closure = s.pop()
            args = s.pop()
            d.append((s, e, code[i:]))
            new_code, new_env = closure
            if new_env[0] != dummy_frame:
                error('No dummy frame.')

            # replace dummy frame with actual frame
            new_env[0] = args

            s, e, code = [], new_env, new_code
            i = 0
        else:
            error(f'Unknown code: {instr}')

    return None


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
    exit_code = run(code)
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
