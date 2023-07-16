#!/usr/bin/env python3

import sys
import argparse
from read import read, ParseError
from machinetypes import Bool, Char, List, Nil, Pair, Symbol, String
from assemble import assemble
from secd import RunError, Secd, UserError
from utils import format_user_error


macros = {}
toplevel_code = []
defined_symbols = set()
set_symbols = set()
read_symbols = set()


def S(s: str) -> Symbol:
    return Symbol(s)


primcalls = {
    'call-with-current-continuation': {
        'nargs': 1,
        'code': [S('ccc')],
    },
    'call/cc': 'call-with-current-continuation',
    'print': {
        'nargs': 1,
        'code': [S('print')],
    },
    'printc': {
        'nargs': 1,
        'code': [S('printc')],
    },
    '#$iadd': {
        'nargs': 2,
        'code': [S('iadd')],
    },
    '#$isub': {
        'nargs': 2,
        'code': [S('isub')],
    },
    '#$imul': {
        'nargs': 2,
        'code': [S('imul')],
    },
    '#$idiv': {
        'nargs': 2,
        'code': [S('idiv')],
    },
    '#$irem': {
        'nargs': 2,
        'code': [S('irem')],
    },
    '#$ilt': {
        'nargs': 2,
        'code': [S('ilt')],
    },
    '#$ile': {
        'nargs': 2,
        'code': [S('ile')],
    },
    '#$shr': {
        'nargs': 2,
        'code': [S('shr')],
    },
    '#$shl': {
        'nargs': 2,
        'code': [S('shl')],
    },
    '#$asr': {
        'nargs': 2,
        'code': [S('asr')],
    },
    '#$bnot': {
        'nargs': 1,
        'code': [S('bnot')],
    },
    '#$band': {
        'nargs': 2,
        'code': [S('band')],
    },
    '#$bor': {
        'nargs': 2,
        'code': [S('bor')],
    },
    '#$bxor': {
        'nargs': 2,
        'code': [S('bxor')],
    },
    'cons': {
        'nargs': 2,
        'code': [S('cons')],
    },
    'car': {
        'nargs': 1,
        'code': [S('car')],
    },
    'cdr': {
        'nargs': 1,
        'code': [S('cdr')],
    },
    'set-car!': {
        'nargs': 2,
        'code': [S('setcar')],
    },
    'set-cdr!': {
        'nargs': 2,
        'code': [S('setcdr')],
    },
    'type': {
        'nargs': 1,
        'code': [S('type')],
    },
    'eq?': {
        'nargs': 2,
        'code': [S('eq')],
    },
    'gensym': {
        'nargs': 0,
        'code': [S('gensym')],
    },
    'char->integer': {
        'nargs': 1,
        'code': [S('ch2i')],
    },
    'integer->char': {
        'nargs': 1,
        'code': [S('i2ch')],
    },
    'char-general-category': {
        'nargs': 1,
        'code': [S('ugcat')],
    },
    'char-upcase': {
        'nargs': 1,
        'code': [S('chup')],
    },
    'char-downcase': {
        'nargs': 1,
        'code': [S('chdn')],
    },
    'char-foldcase': {
        'nargs': 1,
        'code': [S('chfd')],
    },
    'digit-value': {
        'nargs': 1,
        'code': [S('chdv')],
    },
    '#$make-string': {
        'nargs': 2,
        'code': [S('mkstr')],
    },
    'string-ref': {
        'nargs': 2,
        'code': [S('strref')],
    },
    'string-set!': {
        'nargs': 3,
        'code': [S('strset')],
    },
    'string-length': {
        'nargs': 1,
        'code': [S('strlen')],
    },
}


class CompileError(Exception):
    pass


class Macro:
    def __init__(self, name, lambda_form, env):
        self.name = name
        self.lambda_form = lambda_form
        self.env = env

    def expand(self, args):
        global toplevel_code

        quoted_args = [
            List.from_list([S('quote'), a])
            for a in args
        ]
        quoted_args = List.from_list(quoted_args)

        func_call = Pair(self.lambda_form, quoted_args)
        try:
            func_call_code = compile_form(func_call, self.env)
        except CompileError as e:
            raise CompileError(f'During macro expansion of {self.name}: {e}')

        code = toplevel_code + func_call_code
        assembled = assemble(code)

        machine = Secd(assembled)

        try:
            machine.run()
        except UserError:
            err = machine.s[-1]
            msg = format_user_error(err)
            msg = 'During macro expansion: ' + msg
            raise CompileError(msg)
        except RunError as e:
            raise CompileError(f'Run error during macro expansion: {e}')

        if len(machine.s) == 0:
            raise CompileError(f'Internal error: macro did not return anything')

        expanded = machine.s[-1]
        return expanded


def macro_expand(form):
    while isinstance(form, list) and \
          len(form) > 0 and \
          isinstance(form[0], Symbol):
        macro = macros.get(form[0].name)
        if macro is None:
            break
        form = macro.expand(form[1:])

    return form


def parse_define_form(expr, name):
    if len(expr) < 2:
        raise CompileError(f'Invalid number of arguments for {name}.')

    if isinstance(expr[1], Symbol):
        name = expr[1]
        if len(expr) == 3:
            value = expr[2]
        elif len(expr) == 2:
            # define with no value
            # convert from: (define name)
            # to: (define name (quote ()))
            # which is: (define name . ( quote . (() . ()) ))
            value = Pair(Symbol('quote'), Pair(Nil(), Nil()))
        else:
            raise CompileError(f'Invalid number of arguments for {name}.')
    elif isinstance(expr[1], Pair):
        if len(expr) < 3:  # (define (foo))
            raise CompileError(f'Invalid number of arguments for {name}.')
        if expr[1] == Nil():  # (define () x)
            raise CompileError(f'Malformed {name}.')

        # expr: (define . ((name . params) . body))
        # expr[1] is in this form: (name . params)
        name = expr.cdar().car
        params = expr.cdar().cdr
        body = expr.cddr()

        # form: (lambda . ( params . body ))
        value = Pair(S('lambda'), Pair(params, body))
    else:
        raise CompileError(f'Malformed {name}.')

    return name, value


def process_define_macro(expr, env):
    name, lambda_form = parse_define_form(expr, 'define-macro')
    if len(expr) < 3:
        raise CompileError('Not enough arguments for define-macro')

    name = name.name
    macros[name] = Macro(name, lambda_form, env)

    return []


def compile_int(expr, env):
    return [S('ldc'), expr]


def compile_if(expr, env):
    if len(expr) not in (3, 4):
        raise CompileError(f'Invalid number of arguments for if: {expr}')

    cond_code = compile_form(expr[1], env)
    true_code = compile_form(expr[2], env) + [S('join')]
    if len(expr) == 4:
        false_code = compile_form(expr[3], env) + [S('join')]
    else:
        false_code = [S('nil'), S('join')]
    return cond_code + [S('sel')] + [true_code] + [false_code]


def compile_lambda(expr, env):
    if len(expr) < 3:
        raise CompileError(f'Invalid number of arguments for lambda: {expr}')

    params = expr[1]
    if params == Nil():
        params = []
        rest_param = None
    elif isinstance(params, Symbol):
        # single symbol as parameter list captures all parameters
        rest_param = params
        params = []
    elif params.is_proper():
        params = params.to_list()
        rest_param = None
    else:
        new_params = params.before_dot().to_list()
        rest_param = params.after_dot()
        params = new_params

    for p in params:
        if not isinstance(p, Symbol):
            raise CompileError(f'Invalid parameter name: {p}')

    if rest_param and not isinstance(rest_param, Symbol):
        raise CompileError(f'Invalid parameter name: {rest_param}')

    if rest_param:
        params = params + [rest_param]

    new_env = [params] + env

    # expr: (lambda . (params . body))
    body = expr.cddr()
    body_code = []
    for i, e in enumerate(body):
        body_code += compile_form(e, new_env)
        if i < len(body) - 1:
            body_code.append(S('drop'))

    body_code = body_code + [S('ret')]
    if body_code[-2] == S('ap') or body_code[-2] == S('ap'):
        body_code[-2:] = [S('tap')]

    if rest_param:
        code = [S('ldfx'), [len(params) - 1, body_code]]
    else:
        code = [S('ldf'), body_code]

    return code


def compile_symbol(sym: Symbol, env):
    if sym.name in primcalls:
        func_code = []
        prim = primcalls[sym.name]
        if isinstance(prim, str):
            prim = primcalls[prim]
        for i in range(prim['nargs'] - 1, -1, -1):
            func_code += [S('ld'), [0, i]]
        func_code += prim['code']
        func_code += [S('ret')]
        return [S('ldf'), func_code]

    if sym.name.startswith(':'):
        return [S('ldsym'), sym]

    for i, frame in enumerate(env):
        if sym in frame:
            return [S('ld'), [i, frame.index(sym)]]

    read_symbols.add(sym)
    return [S('get'), sym]


def compile_string(s: String, env):
    return [S('ldstr'), s]


def compile_bool(s: Bool, env):
    if s:
        return [S('true')]
    else:
        return [S('false')]


def compile_char(ch: Char, env):
    return [S('ldc'), ch.char_code, S('i2ch')]


def check_let_bindings(bindings, let_name):
    if not isinstance(bindings, List):
        raise CompileError(f'Invalid bindings list for {let_name}: {bindings}')

    if not bindings.is_proper():
        raise CompileError(f'Invalid {let_name} bindings: {bindings}')

    for pair in bindings:
        if not isinstance(pair, Pair) or len(pair) != 2 or not isinstance(pair[0], Symbol):
            raise CompileError(f'Invalid {let_name} binding: {pair}')


def compile_let(expr, env):
    if len(expr) < 2:
        raise CompileError(f'Invalid number of arguments for let: {expr}')

    bindings = expr[1]
    check_let_bindings(bindings, 'let')

    # let: (let . (bindings . body))
    # bindings: (( a . (value1 . nil) (b . (value2 . nil))))
    vars = List.from_list([b.car for b in bindings])
    values = List.from_list([b.cdar() for b in bindings])
    body = expr.cddr()

    for v in vars:
        if not isinstance(v, Symbol):
            raise CompileError(f'Invalid let variable: {v}')

    # transform let to a lambda call and compile that instead
    # ((lambda . ( params . body )) . args)
    lambda_form = Pair(S('lambda'), Pair(vars, body))
    lambda_call = Pair(lambda_form, values)
    return compile_form(lambda_call, env)


def compile_letrec(expr, env):
    if len(expr) < 2:
        raise CompileError(f'Invalid number of arguments for letrec: {expr}')

    bindings = expr[1]
    check_let_bindings(expr[1], 'letrec')

    # bindings (( a . (value1 . nil) (b . (value2 . nil))))
    vars = List.from_list([b.car for b in bindings])
    values = List.from_list([b.cdar() for b in bindings])
    body = expr.cddr()

    for v in vars:
        if not isinstance(v, Symbol):
            raise CompileError(f'Invalid let variable: {v}')

    secd_code = [S('dum'), S('nil')]
    for v in reversed(values.to_list()):
        secd_code += compile_form(v, [vars] + env) + [S('cons')]

    # ((lambda . ( params . body )) . args)
    lambda_form = Pair(S('lambda'), Pair(vars, body))
    secd_code += compile_form(lambda_form, env)

    secd_code += [S('rap')]

    return secd_code


def compile_func_call(expr, env):
    secd_code = [S('nil')]
    for arg in reversed(expr.to_list()[1:]):
        secd_code += compile_form(arg, env)
        secd_code += [S('cons')]
    secd_code += compile_form(expr[0], env)
    secd_code += [S('ap')]
    return secd_code


def compile_apply(expr, env):
    if len(expr) != 3:
        raise CompileError('Invalid number of arguments for #$apply')

    # compile second argument which should be a list
    secd_code = compile_form(expr[2], env)

    # compile first argument which should be a function
    secd_code += compile_form(expr[1], env)

    secd_code += [S('ap')]
    return secd_code


def compile_define(expr, env):
    name, value = parse_define_form(expr, 'define')

    if env == []:
        if name in defined_symbols:
            raise CompileError(f'Duplicate definition: {name}')
        defined_symbols.add(name)

        code = compile_form(value, env)

        # the "dup" instructions makes sure "define" leaves its value on the
        # stack (because all primitive forms are supposed to have a return
        # value)
        code += [S('dup'), S('set'), name]
    else:
        env[0].append(name)
        code = [S('xp')]
        code += compile_form(value, env)
        code += [S('dup'), S('st'), [0, len(env[0]) - 1]]

    return code


def compile_set(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for set!')

    if not isinstance(expr[1], Symbol):
        raise CompileError(f'Variable name passed to set! not a symbol')

    name = expr[1]
    value = expr[2]
    code = compile_form(value, env)

    # leave the value on the stack as return value of set!
    code += [S('dup')]

    for i, frame in enumerate(env):
        if name in frame:
            code += [S('st'), [i, frame.index(name)]]
            break
    else:
        code += [S('set'), name]
        set_symbols.add(name)

    return code


def compile_quoted_form(form, env):
    if isinstance(form, Nil):
        return [S('nil')]
    elif isinstance(form, Pair):
        car = compile_quoted_form(form.car, env)
        cdr = compile_quoted_form(form.cdr, env)
        return cdr + car + [S('cons')]
    elif isinstance(form, Symbol):
        return [S('ldsym'), form]
    else:
        # other atoms evaluate to themselves, quoted or not
        return compile_form(form, env)


def compile_quote(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for quote.')

    return compile_quoted_form(expr[1], env)


def compile_error(expr, env):
    if len(expr) < 2 or len(expr) % 2 != 0:
        raise CompileError(f'Invalid number of arguments for error')

    error_sym = expr[1]
    if not isinstance(error_sym, Symbol):
        raise CompileError(f'First argument to error must be a symbol')

    error_args = [S(':type'), error_sym]

    # expr: (error :type :kw1 v1 :kw2 v2)
    # i.e.: (error . (:type . (:kw1 . (v1 . (:kw2 . (v2 . nil))))))
    cur = expr.cddr() # :type :kw1 v1 :kw2
    while cur != Nil():
        name = cur.car
        if cur.cdr == Nil():
            raise CompileError('Malformed error arguments')
        value = cur.cdar()
        cur = cur.cddr()
        error_args.append(name)
        error_args.append(value)

    code = [S('nil')]
    for arg in reversed(error_args):
        code += compile_form(arg, env)
        code += [S('cons')]
    code += [S('error')]

    return code


def compile_primcall(expr, env, desc):
    if isinstance(desc, str):
        desc = primcalls[desc]

    nargs = desc['nargs']
    if len(expr) != nargs + 1:
        raise CompileError(f'Invalid number of arguments for "{expr[0]}"')

    code = []
    for i in range(nargs - 1, -1, -1):
        code += compile_form(expr[i + 1], env)

    code += desc['code']
    return code


def compile_list(expr, env):
    if expr == Nil():
        raise CompileError('Empty list is not a valid form')

    if not expr.is_proper():
        raise CompileError(f'Cannot compile improper list: {expr}')

    if isinstance(expr.car, Symbol):
        name = expr.car.name
        if name == S('define-macro'):
            raise CompileError('define-macro only allowed at top-level')

        special_forms = {
            'define': compile_define,
            'set!': compile_set,
            'if': compile_if,
            'lambda': compile_lambda,
            'let': compile_let,
            'letrec': compile_letrec,
            'quote': compile_quote,
            'error': compile_error,
            '#$apply': compile_apply,
        }

        compile_func = special_forms.get(name)
        if name in special_forms:
            compile_func = special_forms[name]
            return compile_func(expr, env)
        elif name in primcalls:
            prim = primcalls[name]
            return compile_primcall(expr, env, prim)
        elif name in macros:
            macro = macros[name]
            new_form = macro.expand(expr.cdr)
            return compile_form(new_form, env)
        else:
            return compile_func_call(expr, env)
    else:
        return compile_func_call(expr, env)


def compile_form(expr, env):
    expr = macro_expand(expr)

    if isinstance(expr, List):
        secd_code = compile_list(expr, env)
    elif isinstance(expr, int):
        secd_code = compile_int(expr, env)
    elif isinstance(expr, Symbol):
        secd_code = compile_symbol(expr, env)
    elif isinstance(expr, String):
        secd_code = compile_string(expr, env)
    elif isinstance(expr, Bool):
        secd_code = compile_bool(expr, env)
    elif isinstance(expr, Char):
        secd_code = compile_char(expr, env)
    else:
        raise CompileError(f'Invalid value: {expr}')

    return secd_code


def compile_toplevel(text):
    global toplevel_code

    offset = 0
    code = []
    toplevel_env = []
    while offset < len(text):
        form, offset = read(text, offset)
        if form is None:  # eof
            break

        form = macro_expand(form)

        if isinstance(form, Pair) and len(form) > 0 and form[0] == S('define-macro'):
            process_define_macro(form, toplevel_env)
            form_code = []
        elif isinstance(form, Pair) and len(form) > 0 and form[0] == S('define'):
            form_code = compile_form(form, toplevel_env)
            toplevel_code += form_code
        else:
            form_code = compile_form(form, toplevel_env)

        if code == []:
            code = form_code
        elif form_code != []:
            code += [S('drop')] + form_code

    if code != []:
        code += [S('drop')]

    for sym in set_symbols:
        if sym not in defined_symbols:
            raise CompileError(f'Symbol {sym} is set at some point but never defined')

    for sym in read_symbols:
        if sym not in defined_symbols:
            raise CompileError(f'Symbol {sym} is read at some point but never defined')

    return code


def main():
    parser = argparse.ArgumentParser(
        description='Run SECD instructions written in sexpr form.')

    parser.add_argument(
        'input', default='-', nargs='?',
        help='Input file. Stdin is used if not specified or a dash (-) '
        'is passed instead. Defaults to reading from stdin.')

    parser.add_argument(
        '--lib', '-l', action='append', default=[],
        help='Add a library file to be loaded before input file.')

    parser.add_argument(
        '--macro-expand', '-m', metavar='EXPR', dest='macro_expr',
        help='Macro-expand the given expression (only first term is '
        'expanded).')

    parser.add_argument(
        '--compile-expr', '-c', metavar='EXPR', dest='compile_expr',
        help='Compile and output assembly for the given expression, '
        'after loading libraries.')

    parser.add_argument(
        '--eval', '-e', metavar='EXPR', dest='eval_expr',
        help='Compile and run the given expression, and print the result.')

    args = parser.parse_args()

    text = ''
    for lib in args.lib:
        with open(lib) as f:
            text += f.read()

    if args.macro_expr:
        compile_toplevel(text)  # compile libs

        try:
            form, _ = read(args.macro_expr)
        except ParseError as e:
            print(f'Parse error during macro expansion: {e}')
            sys.exit(1)

        try:
            result = macro_expand(form)
        except CompileError as e:
            print(e)
            sys.exit(1)
        else:
            print(result)
            sys.exit(0)

    if args.compile_expr:
        compile_toplevel(text)  # compile libs
        form, _ = read(args.compile_expr)
        result = compile_form(form, [])
        print(result)
        sys.exit(0)

    if args.eval_expr:
        text += '\n' + args.eval_expr
        code = compile_toplevel(text)
        code = assemble(code)
        m = Secd(code)
        try:
            m.run()
        except RunError as e:
            print('Error:', e)
            sys.exit(1)
        else:
            if len(m.s) == 0:
                print('Error: no result')
                sys.exit(1)
            elif len(m.s) > 1:
                print('Error: more than one result')
                sys.exit(1)
            print(m.s[-1])

        sys.exit(0)

    if args.input == '-':
        text += sys.stdin.read()
    else:
        with open(args.input) as f:
            text += f.read()

    try:
        secd_code = compile_toplevel(text)
    except ParseError as e:
        print(f'Parse error: {e}', file=sys.stderr)
        sys.exit(1)
    except CompileError as e:
        print(f'Compile error: {e}', file=sys.stderr)
        sys.exit(1)

    secd_code = List.from_list_recursive(secd_code)

    print(secd_code)


if __name__ == '__main__':
    main()
