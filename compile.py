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


def compile_print(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for print: {expr}')
    code = compile_form(expr[1], env)
    return code + [S('print')]


def compile_printc(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for printc: {expr}')
    code = compile_form(expr[1], env)
    return code + [S('printc')]


def compile_halt(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for halt: {expr}')
    code = compile_form(expr[1], env)
    return code + [S('halt')]


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


def compile_iadd(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for iadd: {expr}')

    arg1 = compile_form(expr[1], env)
    arg2 = compile_form(expr[2], env)
    return arg1 + arg2 + [S('iadd')]


def compile_isub(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for isub: {expr}')

    arg1 = compile_form(expr[1], env)
    arg2 = compile_form(expr[2], env)
    return arg1 + arg2 + [S('isub')]


def compile_imul(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for imul: {expr}')

    arg1 = compile_form(expr[1], env)
    arg2 = compile_form(expr[2], env)
    return arg1 + arg2 + [S('imul')]


def compile_idiv(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for idiv: {expr}')

    arg1 = compile_form(expr[1], env)
    arg2 = compile_form(expr[2], env)
    return arg1 + arg2 + [S('idiv')]


def compile_irem(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for irem: {expr}')

    arg1 = compile_form(expr[1], env)
    arg2 = compile_form(expr[2], env)
    return arg1 + arg2 + [S('irem')]


def compile_shr(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for shr: {expr}')

    n = compile_form(expr[1], env)
    shift = compile_form(expr[2], env)
    return n + shift + [S('shr')]


def compile_shl(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for shl: {expr}')

    n = compile_form(expr[1], env)
    shift = compile_form(expr[2], env)
    return n + shift + [S('shl')]


def compile_asr(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for asr: {expr}')

    n = compile_form(expr[1], env)
    shift = compile_form(expr[2], env)
    return n + shift + [S('asr')]


def compile_bnot(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for b-not: {expr}')

    n = compile_form(expr[1], env)
    return n + [S('bnot')]


def compile_band(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for b-and: {expr}')

    n1 = compile_form(expr[1], env)
    n2 = compile_form(expr[2], env)
    return n1 + n2 + [S('band')]


def compile_bor(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for b-or: {expr}')

    n1 = compile_form(expr[1], env)
    n2 = compile_form(expr[2], env)
    return n1 + n2 + [S('bor')]


def compile_bxor(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for b-xor: {expr}')

    n1 = compile_form(expr[1], env)
    n2 = compile_form(expr[2], env)
    return n1 + n2 + [S('bxor')]


def compile_lt(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for <: {expr}')

    arg1 = compile_form(expr[1], env)
    arg2 = compile_form(expr[2], env)
    return arg1 + arg2 + [S('lt')]


def compile_le(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for <=: {expr}')

    arg1 = compile_form(expr[1], env)
    arg2 = compile_form(expr[2], env)
    return arg1 + arg2 + [S('le')]


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


def compile_char_to_int(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for char->integer')

    code = compile_form(expr[1], env)
    code += [S('ch2i')]
    return code


def compile_int_to_char(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for integer->char')

    code = compile_form(expr[1], env)
    code += [S('i2ch')]
    return code


def compile_char_general_category(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for char-general-category')

    code = compile_form(expr[1], env)
    code += [S('ugcat')]
    return code


def compile_char_upcase(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for char-upcase')

    code = compile_form(expr[1], env)
    code += [S('chup')]
    return code


def compile_char_downcase(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for char-downcase')

    code = compile_form(expr[1], env)
    code += [S('chdn')]
    return code


def compile_char_foldcase(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for char-foldcase')

    code = compile_form(expr[1], env)
    code += [S('chfd')]
    return code


def compile_digit_value(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for digit-value')

    code = compile_form(expr[1], env)
    code += [S('chdv')]
    return code


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
    if len(expr) < 3:
        raise CompileError('Invalid number of arguments for apply')

    # compile last form, which should be a list
    secd_code = compile_form(expr.last().car, env)

    # prepend all the other arguments to the list
    other_args = expr.to_list()[2:-1]
    for form in reversed(other_args):
        secd_code += compile_form(form, env)
        secd_code += [S('cons')]

    # the function to apply
    secd_code += compile_form(expr[1], env)

    secd_code += [S('ap')]
    return secd_code


def compile_call_cc(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for call/cc')

    secd_code = compile_form(expr[1], env)
    secd_code += [S('ccc')]

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


def compile_cons(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for cons.')

    code = compile_form(expr[2], env)
    code += compile_form(expr[1], env)
    code += [S('cons')]
    return code


def compile_car(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for car.')

    code = compile_form(expr[1], env)
    code += [S('car')]
    return code


def compile_cdr(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for cdr.')

    code = compile_form(expr[1], env)
    code += [S('cdr')]
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


def compile_type(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for type.')

    code = compile_form(expr[1], env)
    code += [S('type')]
    return code


def compile_eq(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for eq.')

    code = compile_form(expr[1], env)
    code += compile_form(expr[2], env)
    code += [S('eq')]
    return code


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


def compile_gensym(expr, env):
    if len(expr) != 1:
        raise CompileError('Invalid number of arguments for gensym')

    return [S('gensym')]


def compile_make_string(expr, env):
    if len(expr) not in (2, 3):
        raise CompileError('Invalid number of arguments for make-string')

    code = []
    if len(expr) == 2:
        # default to filling with null character
        code += [S('ldc'), 0, S('i2ch')]
    else:
        code += compile_form(expr[2], env)

    code += compile_form(expr[1], env)
    code += [S('mkstr')]
    return code


def compile_string_ref(expr, env):
    if len(expr) != 3:
        raise CompileError('Invalid number of arguments for string-ref')

    code = compile_form(expr[1], env)
    code += compile_form(expr[2], env)
    code += [S('strref')]
    return code


def compile_string_set(expr, env):
    if len(expr) != 4:
        raise CompileError('Invalid number of arguments for string-set')

    code = compile_form(expr[1], env)
    code += compile_form(expr[2], env)
    code += compile_form(expr[3], env)
    code += [S('strset')]
    return code


def compile_string_length(expr, env):
    if len(expr) != 2:
        raise CompileError('Invalid number of arguments for string-length')

    code = compile_form(expr[1], env)
    code += [S('strlen')]
    return code


def compile_list(expr, env):
    if expr == Nil():
        raise CompileError('Empty list is not a valid form')

    if not expr.is_proper():
        raise CompileError('Cannot compile improper list')

    if isinstance(expr.car, Symbol):
        name = expr.car.name
        if name == S('define-macro'):
            raise CompileError('define-macro only allowed at top-level')

        primitives = {
            'define': compile_define,
            'set!': compile_set,
            'if': compile_if,
            'iadd': compile_iadd,
            'isub': compile_isub,
            'imul': compile_imul,
            'idiv': compile_idiv,
            'irem': compile_irem,
            'shr': compile_shr,
            'shl': compile_shl,
            'asr': compile_asr,
            'b-not': compile_bnot,
            'b-and': compile_band,
            'b-or': compile_bor,
            'b-xor': compile_bxor,
            '<': compile_lt,
            '<=': compile_le,
            'lambda': compile_lambda,
            'let': compile_let,
            'letrec': compile_letrec,
            'print': compile_print,
            'printc': compile_printc,
            'halt': compile_halt,
            'cons': compile_cons,
            'car': compile_car,
            'cdr': compile_cdr,
            'quote': compile_quote,
            'type': compile_type,
            'eq?': compile_eq,
            'error': compile_error,
            'gensym': compile_gensym,
            'apply': compile_apply,
            'call/cc': compile_call_cc,
            'call-with-current-continuation': compile_call_cc,
            'char->integer': compile_char_to_int,
            'integer->char': compile_int_to_char,
            'char-general-category': compile_char_general_category,
            'char-upcase': compile_char_upcase,
            'char-downcase': compile_char_downcase,
            'char-foldcase': compile_char_foldcase,
            'digit-value': compile_digit_value,
            'make-string': compile_make_string,
            'string-ref': compile_string_ref,
            'string-set!': compile_string_set,
            'string-length': compile_string_length,
        }
        compile_func = primitives.get(name)
        if compile_func is not None:
            return compile_func(expr, env)
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
