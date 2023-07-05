#!/usr/bin/env python3

import sys
import argparse
from read import read, ParseError, print_sexpr
from machinetypes import Symbol


class CompileError(Exception):
    pass


def compile_int(expr, env):
    return ['ldc', expr]


def compile_print(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for print: {expr}')
    code = compile_form(expr[1], env)
    return code + ['print']


def compile_printc(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for printc: {expr}')
    code = compile_form(expr[1], env)
    return code + ['printc']


def compile_halt(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for halt: {expr}')
    code = compile_form(expr[1], env)
    return code + ['halt']


def compile_if(expr, env):
    if len(expr) != 4:
        raise CompileError(f'Invalid number of arguments for if: {expr}')

    cond_code = compile_form(expr[1], env)
    true_code = compile_form(expr[2], env) + ['join']
    false_code = compile_form(expr[3], env) + ['join']
    return cond_code + ['sel'] + [true_code] + [false_code]


def compile_add(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for +: {expr}')

    arg1 = compile_form(expr[1], env)
    arg2 = compile_form(expr[2], env)
    return arg1 + arg2 + ['add']


def compile_sub(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for -: {expr}')

    arg1 = compile_form(expr[1], env)
    arg2 = compile_form(expr[2], env)
    return arg1 + arg2 + ['sub']


def compile_lt(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for <: {expr}')

    arg1 = compile_form(expr[1], env)
    arg2 = compile_form(expr[2], env)
    return arg1 + arg2 + ['lt']


def compile_lambda(expr, env):
    if len(expr) < 2:
        raise CompileError(f'Invalid number of arguments for lambda: {expr}')

    params = expr[1]
    for p in params:
        if not isinstance(p, Symbol):
            raise CompileError(f'Invalid parameter name: {p}')
    new_env = [params] + env

    body = expr[2:]
    if len(body) == 0:
        body = [Symbol('nil')]

    body_code = []
    for i, e in enumerate(body):
        body_code += compile_form(e, new_env)
        if i < len(body) - 1:
            body_code.append('drop')

    body_code = body_code + ['ret']
    if body_code[-2] == 'ap':
        body_code[-2:] = ['tap']

    code = ['ldf', body_code]

    return code


def compile_symbol(sym: Symbol, env):
    if sym.name == 'nil':
        return ['nil']
    elif sym.name == '#f':
        return ['false']
    elif sym.name == '#t':
        return ['true']

    for i, frame in enumerate(env):
        if sym in frame:
            return ['ld', [i, frame.index(sym)]]
    raise CompileError(f'Unknown symbol: {sym}')


def compile_let(expr, env):
    if len(expr) < 2:
        raise CompileError(f'Invalid number of arguments for let: {expr}')

    if not isinstance(expr[1], list):
        raise CompileError(f'Invalid bindings list for let: {expr[1]}')

    bindings = expr[1]
    for pair in bindings:
        if not isinstance(pair, list) or len(pair) != 2 or not isinstance(pair[0], Symbol):
            raise CompileError(f'Invalid let binding: {pair}')

    params = [x for x, y in bindings]
    args = [y for x, y in bindings]
    body = expr[2:]

    for p in params:
        if not isinstance(p, Symbol):
            raise CompileError(f'Invalid let variable: {p}')

    # transform let to a lambda call and compile that instead
    new_expr = [[Symbol('lambda'), params] + body] + args
    return compile_form(new_expr, env)


def compile_letrec(expr, env):
    if len(expr) < 2:
        raise CompileError(f'Invalid number of arguments for letrec: {expr}')

    if not isinstance(expr[1], list):
        raise CompileError(f'Invalid bindings list for letrec: {expr[1]}')

    bindings = expr[1]
    for pair in bindings:
        if not isinstance(pair, list) or \
           len(pair) != 2 or \
           not isinstance(pair[0], Symbol):
            raise CompileError(f'Invalid letrec binding: {pair}')

    vars = [x for x, y in bindings]
    values = [y for x, y in bindings]
    body = expr[2:]

    for v in vars:
        if not isinstance(v, Symbol):
            raise CompileError(f'Invalid let variable: {v}')

    secd_code = ['dum', 'nil']
    for v in values:
        secd_code += compile_form(v, [[v for v in vars]] + env) + ['cons']

    lambda_form = [Symbol('lambda'), vars] + body
    secd_code += compile_form(lambda_form, env)

    secd_code += ['rap']

    return secd_code


def compile_func_call(expr, env):
    secd_code = ['nil']
    for arg in expr[1:]:
        secd_code += compile_form(arg, env)
        secd_code += ['cons']
    secd_code += compile_form(expr[0], env)
    secd_code += ['ap']
    return secd_code


def compile_define(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for define.')

    if not isinstance(expr[1], Symbol):
        raise CompileError(f'Variable name not a symbol.')

    name, value = expr[1:]
    env[0].append(name)
    code = ['xp']
    code += compile_form(value, env)

    # the "dup" instructions makes sure "define" leaves its value on the stack
    # (because all primitive forms are supposed to have a return value)
    code += ['dup', 'st', [0, len(env[0]) - 1]]

    return code


def compile_cons(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for cons.')

    code = compile_form(expr[2], env)
    code += compile_form(expr[1], env)
    code += ['cons']
    return code


def compile_list(expr, env):
    if len(expr) == 0:
        return ['nil']

    if isinstance(expr[0], Symbol):
        name = expr[0].name
        primitives = {
            'define': compile_define,
            'if': compile_if,
            '+': compile_add,
            '-': compile_sub,
            '<': compile_lt,
            'lambda': compile_lambda,
            'let': compile_let,
            'letrec': compile_letrec,
            'print': compile_print,
            'printc': compile_printc,
            'halt': compile_halt,
            'cons': compile_cons,
        }
        compile_func = primitives.get(name)
        if compile_func is not None:
            return  compile_func(expr, env)
        else:
            return compile_func_call(expr, env)
    else:
        return compile_func_call(expr, env)


def symbolize(code):
    "recursively convert all strings in the given list to symbols."
    ret = []
    for i in code:
        if isinstance(i, str):
            ret.append(Symbol(i))
        elif isinstance(i, list):
            ret.append(symbolize(i))
        elif isinstance(i, (int, Symbol)):
            ret.append(i)
        else:
            raise CompileError(f'Internal error: bad secd code: {code}')
    return ret


def compile_form(expr, env=None):
    if env is None:
        # a single nil frame
        env = [[]]

    if isinstance(expr, list):
        secd_code = compile_list(expr, env)
    elif isinstance(expr, int):
        secd_code = compile_int(expr, env)
    elif isinstance(expr, Symbol):
        secd_code = compile_symbol(expr, env)
    else:
        raise CompileError(f'Invalid value: {expr}')

    secd_code = symbolize(secd_code)

    return secd_code


def compile_toplevel(text):
    offset = 0
    code = []
    toplevel_env = [[]]
    first = True
    while offset < len(text):
        form, offset = read(text, offset)
        if form is None:  # eof
            break
        code += compile_form(form, toplevel_env)
        if first:
            first = False
        else:
            code += ['drop']

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
        secd_code = compile_toplevel(text)
    except ParseError as e:
        print(f'Parse error: {e}', file=sys.stderr)
        sys.exit(1)
    except CompileError as e:
        print(f'Compile error: {e}', file=sys.stderr)
        sys.exit(1)

    print_sexpr(secd_code)


if __name__ == '__main__':
    main()
