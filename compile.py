#!/usr/bin/env python3

import sys
import argparse
from read import read, ParseError, print_sexpr
from machinetypes import Symbol, String
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
get_symbols = set()


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

        quoted_args = [[symtab.intern('quote'), a] for a in args]

        func = [symtab.intern('lambda'), self.params] + self.body
        func_call = [func] + quoted_args
        try:
            func_call_code = compile_form(func_call, self.env)
        except CompileError as e:
            raise CompileError(f'During macro expansion of {self.name}: {e}')

        code = toplevel_code + func_call_code
        code = add_tables(code)
        code = symbolize(code)
        assembled = assemble(code)

        machine = Secd(assembled)

        try:
            machine.run()
        except UserError:
            err = machine.s[-1]
            err_type = assoc(symtab.intern(':type'), err)
            msg = f'User error of type {err_type} during macro expansion'
            err_msg = assoc(symtab.intern(':msg'), err)
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

    rest_param = False
    amp = symtab.intern('&')
    if amp in params:
        idx = params.index(amp)
        if idx != len(params) - 2:
            raise CompileError(f'Bad position for & in parameter list')
        params = params[:-2] + [params[-1]]
        rest_param = True

    new_env = [params] + env

    body = expr[2:]
    if len(body) == 0:
        body = [symtab.intern('nil')]

    body_code = []
    for i, e in enumerate(body):
        body_code += compile_form(e, new_env)
        if i < len(body) - 1:
            body_code.append('drop')

    body_code = body_code + ['ret']
    if body_code[-2] == 'ap' or body_code[-2] == symtab.intern('ap'):
        body_code[-2:] = ['tap']

    if rest_param:
        code = ['ldfx', [len(params) - 1, body_code]]
    else:
        code = ['ldf', body_code]


    return code


def compile_symbol(sym: Symbol, env):
    if sym.name == 'nil':
        return ['nil']
    elif sym.name == '#f':
        return ['false']
    elif sym.name == '#t':
        return ['true']
    elif sym.name.startswith(':'):
        return ['ldsym', sym.interned_form]

    for i, frame in enumerate(env):
        if sym in frame:
            return ['ld', [i, frame.index(sym)]]

    get_symbols.add(sym)
    return ['get', sym.interned_form]


def compile_string(s: String, env):
    if s not in strtab:
        strtab.append(s)
        idx = len(strtab) - 1
    else:
        idx = strtab.index(s)

    return ['ldstr', idx]


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
    new_expr = [[symtab.intern('lambda'), params] + body] + args
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
        secd_code += compile_form(v, [vars] + env) + ['cons']

    lambda_form = [symtab.intern('lambda'), vars] + body
    secd_code += compile_form(lambda_form, env)

    secd_code += ['rap']

    return secd_code


def compile_func_call(expr, env):
    secd_code = ['nil']
    for arg in reversed(expr[1:]):
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

    if env == []:
        if name in defined_symbols:
            raise CompileError(f'Duplicate definition: {name}')
        defined_symbols.add(name)

        code = compile_form(value, env)

        # the "dup" instructions makes sure "define" leaves its value on the
        # stack (because all primitive forms are supposed to have a return
        # value)
        code += ['dup', 'set', name.interned_form]
    else:
        env[0].append(name)
        code = ['xp']
        code += compile_form(value, env)
        code += ['dup', 'st', [0, len(env[0]) - 1]]

    return code


def compile_set(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for set!')

    if not isinstance(expr[1], Symbol):
        raise CompileError(f'Variable name passed to set! not a symbol')

    name, value = expr[1:]
    code = compile_form(value, env)

    # leave the value on the stack as return value of set!
    code += ['dup']

    for i, frame in enumerate(env):
        if name in frame:
            code += ['st', [i, frame.index(name)]]
            break
    else:
        code += ['set', name.interned_form]
        set_symbols.add(name)

    return code


def compile_cons(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for cons.')

    code = compile_form(expr[2], env)
    code += compile_form(expr[1], env)
    code += ['cons']
    return code


def compile_car(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for car.')

    code = compile_form(expr[1], env)
    code += ['car']
    return code


def compile_cdr(expr, env):
    if len(expr) != 2:
        raise CompileError(f'Invalid number of arguments for cdr.')

    code = compile_form(expr[1], env)
    code += ['cdr']
    return code


def compile_quoted_form(form, env):
    if isinstance(form, list):
        if form == []:
            return ['nil']
        else:
            first = compile_quoted_form(form[0], env)
            rest = compile_quoted_form(form[1:], env)
            return rest + first + ['cons']
    elif isinstance(form, Symbol):
        n = form.interned_form
        return ['ldsym', n]
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
    code += ['type']
    return code


def compile_eq(expr, env):
    if len(expr) != 3:
        raise CompileError(f'Invalid number of arguments for eq.')

    code = compile_form(expr[1], env)
    code += compile_form(expr[2], env)
    code += ['eq']
    return code


def compile_error(expr, env):
    if len(expr) < 2 or len(expr) % 2 != 0:
        raise CompileError(f'Invalid number of arguments for error')

    error_sym = expr[1]
    if not isinstance(error_sym, Symbol):
        raise CompileError(f'First argument to error must be a symbol')

    error_args = [symtab.intern(':type'), error_sym]
    for i in range(2, len(expr), 2):
        name, value = expr[i:i+2]
        if not isinstance(name, Symbol):
            raise CompileError(f'Error argument name not a symbol: {name}')
        if name == ':type':
            raise CompileError(f'Error argument name must not be :type')
        error_args.append(name)
        error_args.append(value)

    code = ['nil']
    for arg in reversed(error_args):
        code += compile_form(arg, env)
        code += ['cons']
    code += ['error']

    return code


def compile_gensym(expr, env):
    if len(expr) != 1:
        raise CompileError('Invalid number of arguments for gensym')

    return ['gensym']


def compile_list(expr, env):
    if len(expr) == 0:
        return ['nil']

    if isinstance(expr[0], Symbol):
        name = expr[0].name
        if name == 'defmac':
            raise CompileError('defmac only allowed at top-level')

        primitives = {
            'define': compile_define,
            'set!': compile_set,
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
            'car': compile_car,
            'cdr': compile_cdr,
            'quote': compile_quote,
            'type': compile_type,
            'eq?': compile_eq,
            'error': compile_error,
            'gensym': compile_gensym,
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


def symbolize(code):
    "recursively convert all strings in the given list to symbols."
    ret = []
    for i in code:
        if isinstance(i, str):
            ret.append(symtab.intern(i))
        elif isinstance(i, list):
            ret.append(symbolize(i))
        elif isinstance(i, (int, Symbol, String)):
            ret.append(i)
        else:
            raise CompileError(f'Internal error: bad secd code: {code}')
    return ret


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
    else:
        raise CompileError(f'Invalid value: {expr}')

    secd_code = symbolize(secd_code)

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
        code = [symtab.intern('symtab'), strnums] + code

    # add string table
    if len(strtab) > 1:
        # strip the empty string at 0
        code = [symtab.intern('strtab'), strtab[1:]] + code

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

        if isinstance(form, list) and len(form) > 0 and form[0] == symtab.intern('defmac'):
            process_defmac(form, toplevel_env)
            form_code = []
        elif isinstance(form, list) and len(form) > 0 and form[0] == symtab.intern('define'):
            form_code = compile_form(form, toplevel_env)
            toplevel_code += form_code
        else:
            form_code = compile_form(form, toplevel_env)

        if code == []:
            code = form_code
        elif form_code != []:
            code += ['drop'] + form_code

    code = add_tables(code)

    for sym in set_symbols:
        if sym not in defined_symbols:
            raise CompileError(f'Symbol {sym} is set at some point but never defined')

    for sym in get_symbols:
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

    args = parser.parse_args()

    text = ''
    for lib in args.lib:
        with open(lib) as f:
            text += f.read()

    if args.macro_expr:
        compile_toplevel(text)
        form, _ = read(args.macro_expr, symtab=symtab)
        result = macro_expand(form)
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
