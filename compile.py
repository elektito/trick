#!/usr/bin/env python3

import sys
import argparse
from read import read, ParseError, print_sexpr
from machinetypes import Bool, Symbol, String
from assemble import assemble
from secd import RunError, Secd, UserError
from symtab import Symtab


# strtab has an implied empty string in the 0th position
strtab = [String('')]
symtab = Symtab()
macros = {}
toplevel_code = []
defined_symbols = set()
set_symbols = set()
read_symbols = set()


def S(s: str) -> Symbol:
    return symtab.intern(s)


class CompileError(Exception):
    pass


class Macro:
    def __init__(self, name, params, body, env):
        self.name = name
        self.params = params
        self.body = body
        self.env = env

    def expand(self, args):
        global toplevel_code

        quoted_args = [[S('quote'), a] for a in args]

        func = [S('lambda'), self.params] + self.body
        func_call = [func] + quoted_args
        try:
            func_call_code = compile_form(func_call, self.env)
        except CompileError as e:
            raise CompileError(f'During macro expansion of {self.name}: {e}')

        code = toplevel_code + func_call_code
        code = add_tables(code)
        assembled = assemble(code)

        machine = Secd(assembled)

        try:
            machine.run()
        except UserError:
            err = machine.s[-1]
            err_type = assoc(S(':type'), err)
            msg = f'User error of type {err_type} during macro expansion'
            err_msg = assoc(S(':msg'), err)
            if err_msg is not None:
                msg += f': {err_msg}'
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


def assoc(item, alist):
    for i in range(0, len(alist), 2):
        if alist[i] == item:
            return alist[i + 1]

    return None


def process_defmac(expr, env):
    if len(expr) < 3:
        raise CompileError('Not enough arguments for defmac')

    name = expr[1].name
    params = expr[2]
    body = expr[3:]

    if not all(isinstance(p, Symbol) for p in params):
        raise CompileError('Bad macro parameter name')

    macros[name] = Macro(name, params, body, env)

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
    if len(expr) != 4:
        raise CompileError(f'Invalid number of arguments for if: {expr}')

    cond_code = compile_form(expr[1], env)
    true_code = compile_form(expr[2], env) + [S('join')]
    false_code = compile_form(expr[3], env) + [S('join')]
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
    if len(expr) < 2:
        raise CompileError(f'Invalid number of arguments for lambda: {expr}')

    params = expr[1]
    for p in params:
        if not isinstance(p, Symbol):
            raise CompileError(f'Invalid parameter name: {p}')

    rest_param = False
    amp = S('&')
    if amp in params:
        idx = params.index(amp)
        if idx != len(params) - 2:
            raise CompileError(f'Bad position for & in parameter list')
        params = params[:-2] + [params[-1]]
        rest_param = True

    new_env = [params] + env

    body = expr[2:]
    if len(body) == 0:
        body = [S('nil')]

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
    if sym.name == 'nil':
        return [S('nil')]
    elif sym.name.startswith(':'):
        return [S('ldsym'), sym.interned_form]

    for i, frame in enumerate(env):
        if sym in frame:
            return [S('ld'), [i, frame.index(sym)]]

    read_symbols.add(sym)
    return [S('get'), sym.interned_form]


def compile_string(s: String, env):
    if s not in strtab:
        strtab.append(s)
        idx = len(strtab) - 1
    else:
        idx = strtab.index(s)

    return [S('ldstr'), idx]


def compile_bool(s: Bool, env):
    if s:
        return [S('true')]
    else:
        return [S('false')]


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
    new_expr = [[S('lambda'), params] + body] + args
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

    secd_code = [S('dum'), S('nil')]
    for v in values:
        secd_code += compile_form(v, [vars] + env) + [S('cons')]

    lambda_form = [S('lambda'), vars] + body
    secd_code += compile_form(lambda_form, env)

    secd_code += [S('rap')]

    return secd_code


def compile_func_call(expr, env):
    secd_code = [S('nil')]
    for arg in reversed(expr[1:]):
        secd_code += compile_form(arg, env)
        secd_code += [S('cons')]
    secd_code += compile_form(expr[0], env)
    secd_code += [S('ap')]
    return secd_code


def compile_apply(expr, env):
    if len(expr) != 3:
        raise CompileError('Invalid number of arguments for apply')
    secd_code = compile_form(expr[2], env)
    secd_code += compile_form(expr[1], env)
    secd_code += [S('ap')]
    return secd_code


def compile_define(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for define.')

    if not isinstance(expr[1], Symbol):
        raise CompileError(f'Variable name not a symbol.')

    name, value = expr[1:]

    if env == []:
        if name in defined_symbols:
            raise CompileError(f'Duplicate definition: {name}')
        defined_symbols.add(name)

        code = compile_form(value, env)

        # the "dup" instructions makes sure "define" leaves its value on the
        # stack (because all primitive forms are supposed to have a return
        # value)
        code += [S('dup'), S('set'), name.interned_form]
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

    name, value = expr[1:]
    code = compile_form(value, env)

    # leave the value on the stack as return value of set!
    code += [S('dup')]

    for i, frame in enumerate(env):
        if name in frame:
            code += [S('st'), [i, frame.index(name)]]
            break
    else:
        code += [S('set'), name.interned_form]
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
    if isinstance(form, list):
        if form == []:
            return [S('nil')]
        else:
            first = compile_quoted_form(form[0], env)
            rest = compile_quoted_form(form[1:], env)
            return rest + first + [S('cons')]
    elif isinstance(form, Symbol):
        n = form.interned_form
        return [S('ldsym'), n]
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
    for i in range(2, len(expr), 2):
        name, value = expr[i:i+2]
        if not isinstance(name, Symbol):
            raise CompileError(f'Error argument name not a symbol: {name}')
        if name == ':type':
            raise CompileError(f'Error argument name must not be :type')
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


def compile_list(expr, env):
    if len(expr) == 0:
        return [S('nil')]

    if isinstance(expr[0], Symbol):
        name = expr[0].name
        if name == S('defmac'):
            raise CompileError('defmac only allowed at top-level')

        primitives = {
            'define': compile_define,
            'set!': compile_set,
            'if': compile_if,
            'iadd': compile_iadd,
            'isub': compile_isub,
            'imul': compile_imul,
            'idiv': compile_idiv,
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
        }
        compile_func = primitives.get(name)
        if compile_func is not None:
            return compile_func(expr, env)
        elif name in macros:
            macro = macros[name]
            new_form = macro.expand(expr[1:])
            return compile_form(new_form, env)
        else:
            return compile_func_call(expr, env)
    else:
        return compile_func_call(expr, env)


def compile_form(expr, env):
    expr = macro_expand(expr)

    if isinstance(expr, list):
        secd_code = compile_list(expr, env)
    elif isinstance(expr, int):
        secd_code = compile_int(expr, env)
    elif isinstance(expr, Symbol):
        secd_code = compile_symbol(expr, env)
    elif isinstance(expr, String):
        secd_code = compile_string(expr, env)
    elif isinstance(expr, Bool):
        secd_code = compile_bool(expr, env)
    else:
        raise CompileError(f'Invalid value: {expr}')

    return secd_code


def add_tables(code):
    # add the strings for user symbols (those used in quoted values) to the
    # string table
    for name in symtab.interned_names:
        sname = String(name)
        if sname not in strtab:
            strtab.append(sname)

    # add symbol table
    if len(symtab.interned_names) > 0:
        strnums = [
            strtab.index(String(name))
            for name in symtab.interned_names
        ]
        code = [S('symtab'), strnums] + code

    # add string table
    if len(strtab) > 1:
        # strip the empty string at 0
        code = [S('strtab'), strtab[1:]] + code

    return code


def compile_toplevel(text):
    global toplevel_code

    offset = 0
    code = []
    toplevel_env = []
    while offset < len(text):
        form, offset = read(text, offset, symtab=symtab)
        if form is None:  # eof
            break

        form = macro_expand(form)

        if isinstance(form, list) and len(form) > 0 and form[0] == S('defmac'):
            process_defmac(form, toplevel_env)
            form_code = []
        elif isinstance(form, list) and len(form) > 0 and form[0] == S('define'):
            form_code = compile_form(form, toplevel_env)
            toplevel_code += form_code
        else:
            form_code = compile_form(form, toplevel_env)

        if code == []:
            code = form_code
        elif form_code != []:
            code += [S('drop')] + form_code

    code = add_tables(code)

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

    args = parser.parse_args()

    text = ''
    for lib in args.lib:
        with open(lib) as f:
            text += f.read()

    if args.macro_expr:
        compile_toplevel(text)  # compile libs
        form, _ = read(args.macro_expr, symtab=symtab)
        result = macro_expand(form)
        print_sexpr(result)
        sys.exit(0)

    if args.compile_expr:
        compile_toplevel(text)  # compile libs
        form, _ = read(args.compile_expr, symtab=symtab)
        result = compile_form(form, [])
        print_sexpr(result)
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

    print_sexpr(secd_code)


if __name__ == '__main__':
    main()
