#!/usr/bin/env python3

import io
import re
import sys
import argparse

import runtime
from fasl import DefineInfo, Fasl
from read import Reader, ReadError
from machinetypes import Bool, Char, Integer, List, Nil, Pair, Symbol, String, Vector
from assemble import Assembler
from secd import RunError, Secd, UserError
from utils import find_shared, format_user_error


def S(s: str) -> Symbol:
    return Symbol(s)


primcalls = {
    '#$call/cc': {
        'nargs': 1,
        'code': [S('ccc')],
    },
    '#$values->list': {
        'nargs': 1,
        'code': [S('m2l')],
    },
    '#$list->values': {
        'nargs': 1,
        'code': [S('l2m')],
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
        'code': [S('setcar'), S('false')],
    },
    'set-cdr!': {
        'nargs': 2,
        'code': [S('setcdr'), S('false')],
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
        'code': [S('strset'), S('false')],
    },
    'string-length': {
        'nargs': 1,
        'code': [S('strlen')],
    },
    'symbol->string': {
        'nargs': 1,
        'code': [S('sym2str')],
    },
    'string->symbol': {
        'nargs': 1,
        'code': [S('str2sym')],
    },
    '#$make-vector': {
        'nargs': 2,
        'code': [S('mkvec')],
    },
    'vector-ref': {
        'nargs': 2,
        'code': [S('vecref')],
    },
    '#$vector-set!': {
        'nargs': 3,
        'code': [S('vecset'), S('false')],
    },
    'vector-length': {
        'nargs': 1,
        'code': [S('veclen')],
    },
}


def get_primcall(name):
    if name in primcalls:
        ret = primcalls[name]
        if isinstance(ret, str):
            # primitive is a synonoym of another one. look it up again.
            ret = primcalls[ret]
        return ret

    if name.startswith('#$/'):
        m = re.match(r'#\$/(?P<module>\w+)/(?P<proc>\w+)', name)
        if m:
            module = m.group('module')
            proc = m.group('proc')
            desc = runtime.find_proc(module, proc)
            if desc is None:
                raise CompileError(f'Unknown runtime procedure: {name}')

            return {
                'nargs': len(desc['args']),
                'code': [S('trap'), S(f'{module}/{proc}')],
            }

    return None


class CompileError(Exception):
    pass


class Compiler:
    def __init__(self, libs, debug_info=False):
        self.assembler = Assembler()
        self.macros = []
        self.defined_symbols: dict[Symbol, DefineInfo] = {}
        self.set_symbols = set()
        self.read_symbols = set()
        self.defines_fasl = Fasl()
        self.libs = libs
        self.debug_info = debug_info

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
            if isinstance(form, Pair) and not form.is_proper():
                raise CompileError(f'Macro {macro_name_sym} returned an improper list: {form}')

            if isinstance(form, Pair) and not form.is_proper():
                raise CompileError(f'Macro {macro_name_sym} returned an improper list: {form}')

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
        fasl = Fasl()
        func_call_code = [S('get'), name_sym, S('ap')]
        self.assembler.assemble(func_call_code, fasl)

        machine = Secd()

        try:
            # since we want to push arguments on the stack, load the libraries
            # first to make sure they won't interfere with what we push.
            machine.load_fasls(self.libs + [self.defines_fasl])
        except UserError:
            err = machine.s.top()
            msg = format_user_error(err)
            msg = f'When loading libs for macro expansion of {name_sym}: {msg}'
            raise CompileError(msg)
        except RunError as e:
            raise CompileError(f'Run error when loading libs for macro expansion of "{name_sym}": {e}')

        # push the arguments directly on the machine stack. this is a better
        # approach than generating code for the quoted forms of the arguments.
        # not only it requires less code, but also we're passing the exact same
        # code objects/lists we have here in the compiler, and if they are
        # incorporated in the macro output, they still have any source
        # locations/debug info associated with them.
        machine.s.push(args)

        try:
            machine.execute_fasl(fasl)
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

    def parse_define_form(self, expr, form_name):
        if len(expr) < 2:
            raise CompileError(f'Invalid number of arguments for {form_name}.')

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
                raise CompileError(f'Invalid number of arguments for {form_name}.')
        elif isinstance(expr[1], Pair):
            if len(expr) < 3:  # (define (foo))
                raise CompileError(f'Invalid number of arguments for {form_name}.')
            if expr[1] == Nil():  # (define () x)
                raise CompileError(f'Malformed {form_name}.')

            # expr: (define . ((name . params) . body))
            # expr[1] is in this form: (name . params)
            name = expr.cdar().car
            params = expr.cdar().cdr
            body = expr.cddr()

            # form: (lambda . ( params . body ))
            value = Pair(S('lambda'), Pair(params, body))
        else:
            raise CompileError(f'Malformed {form_name}.')

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

        code = self.compile_form(lambda_form, env)
        code += [S('dup'), S('set'), macro_name]

        self.macros.append(macro_name.name)
        
        return code

    def compile_int(self, expr, env):
        return [S('ldc'), Integer(expr)]


    def compile_if(self, expr, env):
        if len(expr) not in (3, 4):
            raise CompileError(f'Invalid number of arguments for if: {expr}')

        cond_code = self.compile_form(expr[1], env)
        true_code = self.compile_form(expr[2], env) + [S('join')]
        if len(expr) == 4:
            false_code = self.compile_form(expr[3], env) + [S('join')]
        else:
            false_code = [S('nil'), S('join')]
        return cond_code + [S('sel')] + [true_code] + [false_code]

    def compile_lambda(self, expr, env):
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
                raise CompileError(f'Invalid parameter name: {p} (not a symbol)')

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
            body_code += self.compile_form(e, new_env)
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
        prim = get_primcall(sym.name)
        if prim:
            # using a primitive like "car" or "cons" as a symbol. this should
            # return a function that performs those primitives. what we do is
            # emit an ldf instruction which loads its arguments into the stack,
            # in the same order as the call to the primitive itself pushes its
            # arguments, and than performs the same instructions the primitive
            # itself would perform. and then returns of course.
            func_code = []
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

    def compile_vector_literal(self, form: Vector, env, labels, processed):
        processed.add(form)
        code = []

        if form in labels:
            for i in range(len(form)):
                code += [S('get'), labels[form]]
                code += [S('ldc'), Integer(i)]
                code += self.compile_quoted_form(form[i], env, labels, processed)
                code += [S('vecset')]
            code += [S('get'), labels[form]]
        else:
            code += [
                S('false'),       # temp fill
                S('ldc'), Integer(len(form)),
                S('mkvec'),
            ]
            for i in range(len(form)):
                code += [S('dup')]
                code += [S('ldc'), Integer(i)]
                code += self.compile_quoted_form(form[i], env, labels, processed)
                code += [S('vecset')]

        return code

    def compile_vector(self, v: Vector, env):
        return self.compile_literal(v, env)

    def check_let_bindings(self, bindings, let_name):
        if not isinstance(bindings, List):
            raise CompileError(f'Invalid bindings list for {let_name}: {bindings}')

        if not bindings.is_proper():
            raise CompileError(f'Invalid {let_name} bindings: {bindings}')

        for pair in bindings:
            if not isinstance(pair, Pair) or len(pair) != 2 or not isinstance(pair[0], Symbol):
                raise CompileError(f'Invalid {let_name} binding: {pair}')

    def compile_let(self, expr, env):
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

            return self.compile_letrec(letrec_expr, env)

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

        return self.compile_form(lambda_call, env)

    def compile_letrec(self, expr, env):
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
            secd_code += self.compile_form(v, [vars] + env) + [S('cons')]

        # ((lambda . ( params . body )) . args)
        lambda_call = Pair(S('lambda'), Pair(vars, body))

        lambda_call.src_start = expr.src_start
        lambda_call.src_end = expr.src_end

        secd_code += self.compile_form(lambda_call, env)
        secd_code += [S('rap')]

        return secd_code

    def compile_func_call(self, expr, env):
        secd_code = [S('nil')]
        for arg in reversed(expr.to_list()[1:]):
            secd_code += self.compile_form(arg, env)
            secd_code += [S('cons')]
        secd_code += self.compile_form(expr[0], env)
        secd_code += [S('ap')]
        return secd_code

    def compile_apply(self, expr, env):
        if len(expr) != 3:
            raise CompileError('Invalid number of arguments for #$apply')

        # compile second argument which should be a list
        secd_code = self.compile_form(expr[2], env)

        # compile first argument which should be a function
        secd_code += self.compile_form(expr[1], env)

        secd_code += [S('ap')]
        return secd_code

    def compile_define(self, expr, env):
        name, value = self.parse_define_form(expr, 'define')

        if env == []:
            self.defined_symbols[name] = DefineInfo(is_macro=False)

            code = self.compile_form(value, env)

            # the "dup" instructions makes sure "define" leaves its value on the
            # stack (because all primitive forms are supposed to have a return
            # value)
            code += [S('dup'), S('set'), name]
        else:
            if name in env[0]:
                raise CompileError(f'Duplicate local definition: {name}')
            env[0].append(name)
            code = [S('xp')]
            code += self.compile_form(value, env)
            code += [S('dup'), S('st'), [0, len(env[0]) - 1]]

        return code

    def compile_set(self, expr, env):
        if len(expr) != 3:
            raise CompileError(f'Invalid number of arguments for set!')

        if not isinstance(expr[1], Symbol):
            raise CompileError(f'Variable name passed to set! not a symbol')

        name = expr[1]
        value = expr[2]
        code = self.compile_form(value, env)

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

    def compile_shared_object(self, form, labels):
        label = Symbol.gensym()
        labels[form] = label

        if isinstance(form, Pair):
            code = [S('false'), S('false'), S('cons'), S('set'), label]
        elif isinstance(form, Vector):
            code = [
                S('false'),  # temp initial fill value
                S('ldc'), Integer(len(form)),
                S('mkvec'),
                S('set'), label,
            ]
        else:
            raise CompileError(
                f'Internal error: do not know how to create object of type: {type(form).__name__}')

        # notice that we should make sure the code generated by this function
        # does not leave anything on the stack

        return code

    def compile_list_literal(self, form, env, labels, processed):
        processed.add(form)
        code = []

        # follow the cdr's until we reach either nil or a shared cell
        pairs = []
        cur = form
        while True:
            pairs.append(cur)
            cur = cur.cdr
            if not isinstance(cur, Pair) or cur in labels:
                break

        # compile the final cdr
        code += self.compile_quoted_form(pairs[-1].cdr, env, labels, processed)

        # now iterate backwards
        for p in reversed(pairs):
            if p in labels:
                code += [S('get'), labels[p], S('setcdr')]
                code += self.compile_quoted_form(p.car, env, labels, processed)
                code += [S('get'), labels[p], S('setcar')]
                code += [S('get'), labels[p]]
            else:
                code += self.compile_quoted_form(p.car, env, labels, processed)
                code += [S('cons')]

        return code

    def compile_quoted_form(self, form, env, labels, processed):
        if form in processed:
            return [S('get'), labels[form]]
        elif isinstance(form, Nil):
            return [S('nil')]
        elif isinstance(form, Pair):
            return self.compile_list_literal(form, env, labels, processed)
        elif isinstance(form, Vector):
            return self.compile_vector_literal(form, env, labels, processed)
        elif isinstance(form, Symbol):
            return [S('ldsym'), form]
        else:
            # other atoms evaluate to themselves, quoted or not
            return self.compile_form(form, env)

    def compile_literal(self, expr, env):
        code = []
        labels = {}
        shared = find_shared(expr)
        for i in shared:
            code += self.compile_shared_object(i, labels)
        processed = set()

        code += self.compile_quoted_form(expr, env, labels, processed)

        for sym in labels.values():
            code += [S('unset'), sym]

        return code

    def compile_quote(self, expr, env):
        if len(expr) != 2:
            raise CompileError(f'Invalid number of arguments for quote.')

        return self.compile_literal(expr[1], env)

    def compile_error(self, expr, env):
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
            code += self.compile_form(arg, env)
            code += [S('cons')]
        code += [S('error')]

        return code

    def compile_primcall(self, expr, env, desc):
        nargs = desc['nargs']
        if len(expr) != nargs + 1:
            raise CompileError(
                f'Invalid number of arguments for "{expr[0]}" '
                f'(expected: {nargs}, got {len(expr)-1})')

        code = []
        for i in range(nargs - 1, -1, -1):
            code += self.compile_form(expr[i + 1], env)

        code += desc['code']
        return code

    def compile_list(self, expr, env):
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
                return compile_func(expr, env)
            elif desc := get_primcall(name):
                return self.compile_primcall(expr, env, desc)
            elif name in self.macros:
                macro = self.macros[name]
                new_form = macro.expand(expr.cdr)
                return self.compile_form(new_form, env)
            else:
                return self.compile_func_call(expr, env)
        else:
            return self.compile_func_call(expr, env)

    def compile_form(self, expr, env):
        if self.detect_cycle(expr):
            raise CompileError(f'Cannot compile cyclic list: {expr}')
        if isinstance(expr, Pair) and not expr.is_proper():
            raise CompileError(f'Cannot compile improper list: {expr}')

        expr = self.macro_expand(expr, env)

        secd_code = []
        if self.debug_info and expr.src_start is not None and expr.src_end is not None:
            secd_code += [S(':expr-start'), Integer(expr.src_start)]

        if isinstance(expr, List):
            secd_code += self.compile_list(expr, env)
        elif isinstance(expr, Integer):
            secd_code += self.compile_int(expr, env)
        elif isinstance(expr, Symbol):
            secd_code += self.compile_symbol(expr, env)
        elif isinstance(expr, String):
            secd_code += self.compile_string(expr, env)
        elif isinstance(expr, Bool):
            secd_code += self.compile_bool(expr, env)
        elif isinstance(expr, Char):
            secd_code += self.compile_char(expr, env)
        elif isinstance(expr, Vector):
            secd_code += self.compile_vector(expr, env)
        else:
            raise CompileError(f'Invalid value: {expr}')

        if self.debug_info and expr.src_start is not None and expr.src_end is not None:
            secd_code += [S(':expr-end'), Integer(expr.src_end)]

        return secd_code

    def detect_cycle(self, form):
        if not isinstance(form, Pair):
            return False

        # we're looking at the "cars" of the form here, because if the cycle is
        # in cdr we'll catch it as an improper list.

        visited = set()
        cur = form
        while True:
            if isinstance(cur, Nil):
                return False
            visited.add(cur)
            if cur.car in visited:
                return True
            if cur.cdr in visited:
                return True
            cur = cur.cdr
            if not isinstance(cur, List):
                return False

    def compile_toplevel(self, text):
        code = []
        toplevel_env = []
        input = io.StringIO(text)
        reader = Reader(input)
        while True:
            try:
                form = reader.read()
            except ReadError as e:
                raise CompileError(f'Read error: {e}')
            if form is None:  # eof
                break

            if self.detect_cycle(form):
                raise CompileError(f'Cannot compile cyclic form: {form}')
            if isinstance(form, Pair) and not form.is_proper():
                raise CompileError(f'Cannot compile improper list: {form}')
            form = self.macro_expand(form, toplevel_env)

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
                form_code = self.compile_form(form, toplevel_env)
                if isinstance(form[1], Symbol):
                    defined_sym = form[1]     # (define foo value)
                else:
                    defined_sym = form[1][0]  # (define (foo . formals) . body)

                if self.debug_info:
                    form_code = \
                        [S(':define-start'), String(defined_sym.name), Integer(form.src_start)] + \
                        form_code + \
                        [S(':define-end'), Integer(form.src_end)]
                self.defines_fasl.add_define(defined_sym, is_macro=False)
                self.assembler.assemble(form_code, self.defines_fasl)
            else:
                form_code = self.compile_form(form, toplevel_env)

            if code == []:
                code = form_code
            elif form_code != []:
                code += [S('drop')] + form_code

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
        '--dbg-info', '-g', action='store_true', default=False,
        help='Add debug info symbols to the compiler output.')

    parser.set_defaults(func=main)


def main(args):
    lib_fasls = []
    for lib in args.lib:
        with open(lib, 'rb') as f:
            lib_fasls.append(Fasl.load(f, lib))

    compiler = Compiler(lib_fasls, debug_info=args.dbg_info)

    if args.input == '-':
        text = sys.stdin.read()
    else:
        with open(args.input) as f:
            text = f.read()

    try:
        secd_code = compiler.compile_toplevel(text)
    except ReadError as e:
        print(f'Read error: {e}', file=sys.stderr)
        sys.exit(1)
    except CompileError as e:
        print(f'Compile error: {e}', file=sys.stderr)
        sys.exit(1)

    secd_code = List.from_list_recursive(secd_code)
    print(secd_code)
