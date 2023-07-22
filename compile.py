#!/usr/bin/env python3

import sys
import argparse
from dbginfo import DebugInfoRecord
from fasl import DefineInfo, Fasl
from read import read, ParseError
from machinetypes import Bool, Char, Integer, List, Nil, Pair, Symbol, String
from assemble import Assembler
from secd import RunError, Secd, UserError
from utils import format_user_error


def S(s: str) -> Symbol:
    return Symbol(s)


primcalls = {
    'call-with-current-continuation': {
        'nargs': 1,
        'code': [S('ccc')],
    },
    'call/cc': 'call-with-current-continuation',
    '#$values->list': {
        'nargs': 1,
        'code': [S('m2l')],
    },
    '#$list->values': {
        'nargs': 1,
        'code': [S('l2m')],
    },
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


class Compiler:
    def __init__(self, libs):
        self.assembler = Assembler()
        self.macros = []
        self.defined_symbols: dict[Symbol, DefineInfo] = {}
        self.set_symbols = set()
        self.read_symbols = set()
        self.dbg_info = []
        self.defines_fasl = Fasl()
        self.libs = libs

        # add library macros to our macro list
        for lib in libs:
            for sym, info in lib.defines.items():
                if info.is_macro:
                    self.macros.append(sym.name)

        # for now, we'll treat everything as a library
        self.compiling_library = True

    def macro_expand(self, form, env):
        while isinstance(form, Pair) and \
              len(form) > 0 and \
              isinstance(form[0], Symbol):
            name_sym = form[0]
            macro_name_sym = S(f'#m:{name_sym.name}')
            if macro_name_sym.name not in self.macros:
                break

            src_start = form.src_start
            src_end = form.src_end

            args = form.cdr
            form = self.expand_single_macro(macro_name_sym, args, env)

            form.src_start = src_start
            form.src_end = src_end

        return form

    def macro_expand_full(self, form, env):
        while isinstance(form, Pair) and \
              len(form) > 0 and \
              isinstance(form[0], Symbol):
            name_sym = form[0]
            macro_name_sym = S(f'#m:{name_sym.name}')
            if macro_name_sym.name not in self.macros:
                break

            args = form.cdr
            form = self.expand_single_macro(macro_name_sym, args, env)

        rest = form.cdr
        while rest != Nil():
            if isinstance(rest.car, Pair):
                rest.car = self.macro_expand_full(rest.car, env)
            rest = rest.cdr

        return form

    def expand_single_macro(self, name_sym, args, env):
        quoted_args = [
            List.from_list([S('quote'), a])
            for a in args
        ]
        quoted_args = List.from_list(quoted_args)

        func_call = Pair(name_sym, quoted_args)

        try:
            func_call_code = self.compile_func_call(
                func_call, env,
                start_offset=0,  # don't care
            )
        except CompileError as e:
            raise CompileError(
                f'Compile error during macro expansion of '
                f'{name_sym}: {e}')

        fasl = Fasl()
        self.assembler.assemble(func_call_code, fasl)

        libs = [self.defines_fasl] + self.libs

        machine = Secd(fasl, libs)

        try:
            machine.run()
        except UserError:
            err = machine.s.top()
            msg = format_user_error(err)
            msg = f'During macro expansion of {name_sym}: {msg}'
            raise CompileError(msg)
        except RunError as e:
            raise CompileError(f'Run error during macro expansion of "{name_sym}": {e}')

        if len(machine.s) == 0:
            raise CompileError(f'Internal error: macro did not return anything')

        expanded = machine.s.top()
        return expanded

    def parse_define_form(self, expr, name):
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

        value.src_start = expr.src_start
        value.src_end = expr.src_end

        return name, value

    def process_define_macro(self, expr, env):
        # we'll be compiling the macro almost exactly the same as we compile a
        # regular top-level define. we only set the define type as a macro in
        # self.defined_symbols.

        name, lambda_form = self.parse_define_form(expr, 'define-macro')
        if len(expr) < 3:
            raise CompileError('Not enough arguments for define-macro')

        # we'll define the macro as a function under another name, in order to
        # make sure the macro is not run as a function by accident (for example,
        # due to the fact that the macro is being used before being defined
        # inside another macro)
        macro_name = S(f'#m:{name}')

        if macro_name in self.macros:
            raise CompileError(f'Duplicate macro definition: {name}')
        self.defined_symbols[macro_name] = DefineInfo(is_macro=True)

        code = self.compile_form(lambda_form, env, start_offset=0)
        code += [S('dup'), S('set'), macro_name]

        self.macros.append(macro_name.name)
        
        return code

    def compile_int(self, expr, env):
        return [S('ldc'), Integer(expr)]


    def compile_if(self, expr, env, start_offset):
        if len(expr) not in (3, 4):
            raise CompileError(f'Invalid number of arguments for if: {expr}')

        cond_code = self.compile_form(expr[1], env, 0)
        true_code = self.compile_form(expr[2], env, 0) + [S('join')]
        if len(expr) == 4:
            false_start_offset = start_offset + len(cond_code) + len(true_code)
            false_code = self.compile_form(expr[3], env, false_start_offset) + [S('join')]
        else:
            false_code = [S('nil'), S('join')]
        return cond_code + [S('sel')] + [true_code] + [false_code]

    def compile_lambda(self, expr, env, start_offset):
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

        # we store the number of parameters here, becaues when we compile the
        # body further down, the params value might change in case there are
        # "define" forms in the body. we will use this original value when
        # writing the "ldf" instruction though.
        original_nparams = len(params)

        new_env = [params] + env

        # expr: (lambda . (params . body))
        body = expr.cddr()
        body_code = []
        for i, e in enumerate(body):
            body_code += self.compile_form(e, new_env, len(body_code))
            if i < len(body) - 1:
                body_code.append(S('drop'))

        body_code = body_code + [S('ret')]
        if body_code[-2] == S('ap') or body_code[-2] == S('ap'):
            body_code[-2:] = [S('tap')]

        if rest_param:
            code = [S('ldf'), Integer(-original_nparams), body_code]
        else:
            code = [S('ldf'), Integer(original_nparams), body_code]

        return code

    def compile_symbol(self, sym: Symbol, env):
        if sym.name in primcalls:
            # using a primitive like "car" or "cons" as a symbol. this should
            # return a function that performs those primitives. what we do is
            # emit an ldf instruction which loads its arguments into the stack,
            # in the same order as the call to the primitive itself pushes its
            # arguments, and than performs the same instructions the primitive
            # itself would perform. and then returns of course.
            func_code = []
            prim = primcalls[sym.name]
            if isinstance(prim, str):
                # primitive is a synonoym of another one. look it up again.
                prim = primcalls[prim]
            for i in range(prim['nargs'] - 1, -1, -1):
                func_code += [S('ld'), [Integer(0), Integer(i)]]
            func_code += prim['code']
            func_code += [S('ret')]
            return [S('ldf'), Integer(prim['nargs']), func_code]

        if sym.name.startswith(':'):
            return [S('ldsym'), sym]

        for i, frame in enumerate(env):
            if sym in frame:
                return [S('ld'), [Integer(i), Integer(frame.index(sym))]]

        self.read_symbols.add(sym)
        return [S('get'), sym]

    def compile_string(self, s: String, env):
        return [S('ldstr'), s]

    def compile_bool(self, s: Bool, env):
        if s:
            return [S('true')]
        else:
            return [S('false')]

    def compile_char(self, ch: Char, env):
        return [S('ldc'), Integer(ch.char_code), S('i2ch')]

    def check_let_bindings(self, bindings, let_name):
        if not isinstance(bindings, List):
            raise CompileError(f'Invalid bindings list for {let_name}: {bindings}')

        if not bindings.is_proper():
            raise CompileError(f'Invalid {let_name} bindings: {bindings}')

        for pair in bindings:
            if not isinstance(pair, Pair) or len(pair) != 2 or not isinstance(pair[0], Symbol):
                raise CompileError(f'Invalid {let_name} binding: {pair}')

    def compile_let(self, expr, env, start_offset):
        if len(expr) < 2:
            raise CompileError(f'Invalid number of arguments for let: {expr}')

        # convert named let to letrec
        if isinstance(expr[1], Symbol):
            if len(expr) < 3:
                raise CompileError(f'Invalid number of arguments for named-let: {expr}')

            # this:
            #    (let f bindings . body)
            # will be converted to
            #    (letrec ((f (lambda ,@(var-names) body) ,@(var-values)))
            #      (f ,@(var-values))

            let_name = expr[1]
            bindings = expr[2]
            let_body = expr.cdr.cdr.cdr
            self.check_let_bindings(bindings, 'named-let')

            var_names = List.from_list([i[0] for i in bindings])
            var_values = [i[1] for i in bindings]

            lambda_expr = List.from_list(
                [S('lambda'), var_names] + let_body.to_list()
            )

            lambda_expr.src_start = expr.src_start
            lambda_expr.src_end = expr.src_end

            letrec_binding = List.from_list([let_name, lambda_expr])
            letrec_bindings = List.from_list([letrec_binding])

            letrec_expr = List.from_list([
                S('letrec'),
                letrec_bindings,
                List.from_list([let_name] + var_values),
            ])

            letrec_expr.src_start = expr.src_start
            letrec_expr.src_end = expr.src_end

            return self.compile_letrec(letrec_expr, env, start_offset)

        bindings = expr[1]
        self.check_let_bindings(bindings, 'let')

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

        lambda_form.src_start = expr.src_start
        lambda_form.src_end = expr.src_end

        lambda_call.src_start = expr.src_start
        lambda_call.src_end = expr.src_end

        return self.compile_form(lambda_call, env, start_offset)

    def compile_letrec(self, expr, env, start_offset):
        if len(expr) < 2:
            raise CompileError(f'Invalid number of arguments for letrec: {expr}')

        bindings = expr[1]
        self.check_let_bindings(expr[1], 'letrec')

        # bindings (( a . (value1 . nil) (b . (value2 . nil))))
        vars = List.from_list([b.car for b in bindings])
        values = List.from_list([b.cdar() for b in bindings])
        body = expr.cddr()

        for v in vars:
            if not isinstance(v, Symbol):
                raise CompileError(f'Invalid let variable: {v}')

        if values != Nil():
            for v, b in zip(values, bindings):
                v.src_start = b.cdar().src_start
                v.src_end = b.cdar().src_end

        secd_code = [S('dum'), S('nil')]
        for v in reversed(values.to_list()):
            v_start_offset = start_offset + len(secd_code)
            secd_code += self.compile_form(v, [vars] + env, v_start_offset) + [S('cons')]

        # ((lambda . ( params . body )) . args)
        lambda_call = Pair(S('lambda'), Pair(vars, body))

        lambda_call.src_start = expr.src_start
        lambda_call.src_end = expr.src_end

        lambda_start_offset = start_offset + len(lambda_call)
        secd_code += self.compile_form(lambda_call, env, lambda_start_offset)

        secd_code += [S('rap')]

        return secd_code

    def compile_func_call(self, expr, env, start_offset):
        secd_code = [S('nil')]
        for arg in reversed(expr.to_list()[1:]):
            arg_start_offset = start_offset + len(secd_code)
            secd_code += self.compile_form(arg, env, arg_start_offset)
            secd_code += [S('cons')]
        func_start_offset = start_offset + len(secd_code)
        secd_code += self.compile_form(expr[0], env, func_start_offset)
        secd_code += [S('ap')]
        return secd_code

    def compile_apply(self, expr, env, start_offset):
        if len(expr) != 3:
            raise CompileError('Invalid number of arguments for #$apply')

        # compile second argument which should be a list
        secd_code = self.compile_form(expr[2], env, start_offset)

        # compile first argument which should be a function
        secd_code += self.compile_form(
            expr[1], env, start_offset + len(secd_code))

        secd_code += [S('ap')]
        return secd_code

    def compile_define(self, expr, env, start_offset):
        name, value = self.parse_define_form(expr, 'define')

        if env == []:
            self.defined_symbols[name] = DefineInfo(is_macro=False)

            code = self.compile_form(value, env, start_offset)

            # the "dup" instructions makes sure "define" leaves its value on the
            # stack (because all primitive forms are supposed to have a return
            # value)
            code += [S('dup'), S('set'), name]
        else:
            if name in env[0]:
                raise CompileError(f'Duplicate local definition: {name}')
            env[0].append(name)
            code = [S('xp')]
            code += self.compile_form(value, env, start_offset + len(code))
            code += [S('dup'), S('st'), [0, len(env[0]) - 1]]

        return code

    def compile_set(self, expr, env, start_offset):
        if len(expr) != 3:
            raise CompileError(f'Invalid number of arguments for set!')

        if not isinstance(expr[1], Symbol):
            raise CompileError(f'Variable name passed to set! not a symbol')

        name = expr[1]
        value = expr[2]
        code = self.compile_form(value, env, start_offset)

        # leave the value on the stack as return value of set!
        code += [S('dup')]

        for i, frame in enumerate(env):
            if name in frame:
                code += [S('st'), [Integer(i), Integer(frame.index(name))]]
                break
        else:
            code += [S('set'), name]
            self.set_symbols.add(name)

        return code

    def compile_quoted_form(self, form, env, start_offset):
        if isinstance(form, Nil):
            return [S('nil')]
        elif isinstance(form, Pair):
            car = self.compile_quoted_form(
                form.car, env, start_offset)
            cdr = self.compile_quoted_form(
                form.cdr, env, start_offset + len(car))
            return cdr + car + [S('cons')]
        elif isinstance(form, Symbol):
            return [S('ldsym'), form]
        else:
            # other atoms evaluate to themselves, quoted or not
            return self.compile_form(form, env, start_offset)

    def compile_quote(self, expr, env, start_offset):
        if len(expr) != 2:
            raise CompileError(f'Invalid number of arguments for quote.')

        return self.compile_quoted_form(expr[1], env, start_offset)

    def compile_error(self, expr, env, start_offset):
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
            code += self.compile_form(arg, env, start_offset + len(code))
            code += [S('cons')]
        code += [S('error')]

        return code

    def compile_primcall(self, expr, env, desc, start_offset):
        if isinstance(desc, str):
            desc = primcalls[desc]

        nargs = desc['nargs']
        if len(expr) != nargs + 1:
            raise CompileError(f'Invalid number of arguments for "{expr[0]}"')

        code = []
        for i in range(nargs - 1, -1, -1):
            code += self.compile_form(
                expr[i + 1], env, start_offset + len(code))

        code += desc['code']
        return code

    def compile_list(self, expr, env, start_offset):
        if expr == Nil():
            raise CompileError('Empty list is not a valid form')

        if not expr.is_proper():
            raise CompileError(f'Cannot compile improper list: {expr}')

        if isinstance(expr.car, Symbol):
            name = expr.car.name
            if name == S('define-macro'):
                raise CompileError('define-macro only allowed at top-level')

            special_forms = {
                'define': self.compile_define,
                'set!': self.compile_set,
                'if': self.compile_if,
                'lambda': self.compile_lambda,
                'let': self.compile_let,
                'letrec': self.compile_letrec,
                'quote': self.compile_quote,
                'error': self.compile_error,
                '#$apply': self.compile_apply,
            }

            compile_func = special_forms.get(name)
            if name in special_forms:
                compile_func = special_forms[name]
                return compile_func(expr, env, start_offset)
            elif name in primcalls:
                prim = primcalls[name]
                return self.compile_primcall(expr, env, prim, start_offset)
            elif name in self.macros:
                macro = self.macros[name]
                new_form = macro.expand(expr.cdr)
                return self.compile_form(new_form, env, start_offset)
            else:
                return self.compile_func_call(expr, env, start_offset)
        else:
            return self.compile_func_call(expr, env, start_offset)

    def compile_form(self, expr, env, start_offset, *, no_dbginfo=False):
        expr = self.macro_expand(expr, env)

        if isinstance(expr, List):
            secd_code = self.compile_list(expr, env, start_offset)
        elif isinstance(expr, Integer):
            secd_code = self.compile_int(expr, env)
        elif isinstance(expr, Symbol):
            secd_code = self.compile_symbol(expr, env)
        elif isinstance(expr, String):
            secd_code = self.compile_string(expr, env)
        elif isinstance(expr, Bool):
            secd_code = self.compile_bool(expr, env)
        elif isinstance(expr, Char):
            secd_code = self.compile_char(expr, env)
        else:
            raise CompileError(f'Invalid value: {expr}')

        if not no_dbginfo:
            self.add_dbginfo_record(
                expr,
                start_offset,
                start_offset + len(secd_code),
            )

        return secd_code

    def add_dbginfo_record(self, form, asm_start, asm_end):
        if form.src_start is None or form.src_end is None:
            return
        self.dbg_info.append(
            DebugInfoRecord(form.src_start, form.src_end, asm_start, asm_end, form))

    def compile_toplevel(self, text):
        src_offset = 0
        asm_offset = 0
        code = []
        toplevel_env = []
        while src_offset < len(text):
            try:
                form, src_offset = read(text, src_offset)
            except ParseError as e:
                raise CompileError(f'Parse error: {e}')
            if form is None:  # eof
                break

            form = self.macro_expand(form, toplevel_env)
            asm_start = len(code)

            if isinstance(form, Pair) and len(form) > 0 and form[0] == S('define-macro'):
                form_code = self.process_define_macro(form, toplevel_env)
                if isinstance(form[1], Symbol):
                    defined_sym = form[1]     # (define-macro foo value)
                else:
                    defined_sym = form[1][0]  # (define-macro (foo . formals) . body)

                self.defines_fasl.add_define(defined_sym, is_macro=True)
                self.assembler.assemble(form_code, self.defines_fasl)

                # if we're not compiling a library, do not include the macro
                # code in the output.
                if not self.compiling_library:
                    form_code = []
            elif isinstance(form, Pair) and len(form) > 0 and form[0] == S('define'):
                form_code = self.compile_form(form, toplevel_env, asm_offset)
                if isinstance(form[1], Symbol):
                    defined_sym = form[1]     # (define foo value)
                else:
                    defined_sym = form[1][0]  # (define (foo . formals) . body)
                self.defines_fasl.add_define(defined_sym, is_macro=False)
                self.assembler.assemble(form_code, self.defines_fasl)
            else:
                form_code = self.compile_form(form, toplevel_env, asm_offset)

            if code == []:
                code = form_code
            elif form_code != []:
                code += [S('drop')] + form_code

            self.add_dbginfo_record(
                form, asm_start, len(code))

        if code != []:
            code += [S('drop')]

        all_defines = set(self.defined_symbols.keys())
        for lib in self.libs:
            all_defines |= set(lib.defines)

        for sym in self.set_symbols:
            macro_name = S(f'#m:{sym.name}')
            if sym not in all_defines and not macro_name in all_defines:
                raise CompileError(
                    f'Symbol {sym} is set at some point but never defined')

        for sym in self.read_symbols:
            macro_name = S(f'#m:{sym.name}')
            if sym not in all_defines and not macro_name in all_defines:
                raise CompileError(f'Symbol {sym} is read at some point but never defined')

        return code


def configure_argparse(parser: argparse.ArgumentParser):
    parser.description = 'Compile Trick scheme program into SECD assembly.'

    parser.add_argument(
        'input', default='-', nargs='?',
        help='Input file. Stdin is used if not specified or a dash (-) '
        'is passed instead. Defaults to reading from stdin.')

    parser.add_argument(
        '--lib', '-l', action='append', default=[],
        help='Add a library FASL to be loaded when compiling.')

    parser.add_argument(
        '--dbg-info', '-g', action='store_true',
        help='Enable debug info.')

    parser.set_defaults(func=main)


def main(args):
    lib_fasls = []
    for lib in args.lib:
        with open(lib, 'rb') as f:
            lib_fasls.append(Fasl.load(f))

    compiler = Compiler(lib_fasls)

    if args.input == '-':
        text = sys.stdin.read()
    else:
        with open(args.input) as f:
            text = f.read()

    try:
        secd_code = compiler.compile_toplevel(text)
    except ParseError as e:
        print(f'Parse error: {e}', file=sys.stderr)
        sys.exit(1)
    except CompileError as e:
        print(f'Compile error: {e}', file=sys.stderr)
        sys.exit(1)

    secd_code = List.from_list_recursive(secd_code)
    print(secd_code)
