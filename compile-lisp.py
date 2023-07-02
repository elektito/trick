#!/usr/bin/env python3

import sys
import argparse
from sexpr import read, ParseError, Symbol, print_sexpr


class CompileError(Exception):
    pass


def compile_lisp_int(expr, env):
    return ['ldc', expr]


def compile_lisp_printn(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for printn: {expr}')
    code = compile_lisp(expr[1], env)
    return code + ['printn']


def compile_lisp_printc(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for printc: {expr}')
    code = compile_lisp(expr[1], env)
    return code + ['printc']


def compile_lisp_halt(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for halt: {expr}')
    code = compile_lisp(expr[1], env)
    return code + ['halt']


def compile_lisp_if(expr, env):
    if len(expr) != 4:
        raise CompileError(f'Invalid number of arguments for if: {expr}')

    cond_code = compile_lisp(expr[1], env)
    true_code = compile_lisp(expr[2], env) + ['join']
    false_code = compile_lisp(expr[3], env) + ['join']
    return cond_code + ['sel'] + [true_code] + [false_code]


def compile_lisp_add(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for +: {expr}')

    arg1 = compile_lisp(expr[1], env)
    arg2 = compile_lisp(expr[2], env)
    return arg1 + arg2 + ['add']


def compile_lisp_sub(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for -: {expr}')

    arg1 = compile_lisp(expr[1], env)
    arg2 = compile_lisp(expr[2], env)
    return arg1 + arg2 + ['sub']


def compile_lisp_lt(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for <: {expr}')

    arg1 = compile_lisp(expr[1], env)
    arg2 = compile_lisp(expr[2], env)
    return arg1 + arg2 + ['lt']


def compile_lisp_lambda(expr, env):
    if len(expr) < 3:
        raise CompileError(f'Invalid number of arguments for lambda: {expr}')

    params = expr[1]
    for p in params:
        if not isinstance(p, Symbol):
            raise CompileError(f'Invalid parameter name: {p}')
    params = [p.name for p in params]
    new_env = [params] + env

    code = []
    for e in expr[2:]:
        code += compile_lisp(e, new_env)

    return ['ldf'] + [code + ['ret']]


def compile_lisp_symbol(expr, env):
    sym = expr.name
    for i, frame in enumerate(env):
        if sym in frame:
            return ['ld', [i, frame.index(sym)]]
    raise CompileError(f'Unknown symbol: {sym}')


def compile_lisp_let(expr, env):
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
    return compile_lisp(new_expr, env)


def compile_lisp_letrec(expr, env):
    if len(expr) < 2:
        raise CompileError(f'Invalid number of arguments for letrec: {expr}')

    if not isinstance(expr[1], list):
        raise CompileError(f'Invalid bindings list for letrec: {expr[1]}')

    bindings = expr[1]
    for pair in bindings:
        if not isinstance(pair, list) or len(pair) != 2 or not isinstance(pair[0], Symbol):
            raise CompileError(f'Invalid letrec binding: {pair}')

    vars = [x for x, y in bindings]
    values = [y for x, y in bindings]
    body = expr[2:]

    for v in vars:
        if not isinstance(v, Symbol):
            raise CompileError(f'Invalid let variable: {v}')

    secd_code = ['dum', 'nil']
    for v in values:
        secd_code += compile_lisp(v, [[v.name for v in vars]] + env) + ['cons']

    lambda_form = [Symbol('lambda'), vars] + body
    secd_code += compile_lisp(lambda_form, env)

    secd_code += ['rap']

    return secd_code


def compile_func_call(expr, env):
    secd_code = ['nil']
    for arg in expr[1:]:
        secd_code += compile_lisp(arg, env)
        secd_code += ['cons']
    secd_code += compile_lisp(expr[0], env)
    secd_code += ['ap']
    return secd_code


def compile_list(expr, env):
    if len(expr) == 0:
        return ['nil']

    if isinstance(expr[0], Symbol):
        name = expr[0].name
        primitives = {
            'if': compile_lisp_if,
            '+': compile_lisp_add,
            '-': compile_lisp_sub,
            '<': compile_lisp_lt,
            'lambda': compile_lisp_lambda,
            'let': compile_lisp_let,
            'letrec': compile_lisp_letrec,
            'printn': compile_lisp_printn,
            'printc': compile_lisp_printc,
            'halt': compile_lisp_halt,
        }
        compile_func = primitives.get(name)
        if compile_func is not None:
            return  compile_func(expr, env)
        else:
            return compile_func_call(expr, env)
    else:
        return compile_func_call(expr, env)


def compile_lisp(expr, env):
    if expr == 'nil':
        secd_code = ['nil']
    elif isinstance(expr, list):
        secd_code = compile_list(expr, env)
    elif isinstance(expr, int):
        secd_code = compile_lisp_int(expr, env)
    elif isinstance(expr, Symbol):
        secd_code = compile_lisp_symbol(expr, env)
    else:
        raise CompileError(f'Invalid value: {expr}')

    return secd_code


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
        expr = read(text)
    except ParseError as e:
        print(f'Parse error: {e}')
        sys.exit(1)

    if not isinstance(expr, list):
        raise CompileError('Input is not a list.')

    secd_code = compile_lisp(expr, [])
    print_sexpr(secd_code)


if __name__ == '__main__':
    main()
