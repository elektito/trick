#!/usr/bin/env python3

from enum import Enum
import io
import os
import re
import platform
import sys
import argparse

import runtime
from fasl import DefineInfo, Fasl
from read import Reader, ReadError
from machinetypes import Bool, Char, Integer, List, Nil, Pair, Symbol, String, Vector
from assemble import Assembler
from secd import RunError, Secd
from snippet import show_snippet
from utils import find_shared
from version import __version__


def S(s: str) -> Symbol:
    return Symbol(s)


def get_features():
    architecture = platform.machine().replace('_', '-')

    extra_features = []
    if sys.platform.startswith('linux'):
        os_name = 'gnu-linux'
        extra_features.append('linux')
    elif sys.platform.startswith('freebsd'):
        os_name = 'freebsd'
        extra_features.append('bsd')
    elif sys.platform == 'win32':
        os_name = 'windows'
    else:
        os_name = sys.platform
        if os_name[-1].isnumeric():
            os_name = os_name[:-1]

    if sys.byteorder == 'little':
        endianness = 'little-endian'
    else:
        endianness = 'big-endian'

    features = [
        'r7rs',
        'trick',
        'trick-' + __version__[:__version__.rindex('.')], # only add major and minor versions
        'full-unicode',
        architecture,
        os_name,
        endianness,
    ]
    features += extra_features

    if os.name == 'posix':
        features.append('posix')

    return features


def asm_code_for_features():
    features = get_features()

    code = [S('nil')]
    for feature in reversed(features):
        code += [S('ldsym'), S(feature), S('cons')]

    return code


class SourceFile:
    """ The purpose of this class is to unify the cases where we have source as
    text or as a filename. """

    def __init__(self, text=None, filename=None):
        assert text or filename
        self._text = text
        self._filename = filename

    @property
    def text(self):
        if self._text:
            return self._text

        if not self.filename:
            return None

        with open(self.filename) as f:
            return f.read()

    @property
    def filename(self):
        return self._filename

    def __repr__(self):
        if self.filename:
            return f'<SourceFile {self._filename}>'
        else:
            return f'<SourceFile>'


class SymbolKind(Enum):
    PRIMCALL = 1
    LOCAL = 2
    FREE = 3


class SymbolInfo:
    def __init__(self, symbol: Symbol, kind: SymbolKind, *,
                 primcall_nargs=None,
                 primcall_code=None,
                 local_frame_idx=None,
                 local_var_idx=None,
                 immutable=False):
        self.symbol = symbol
        self.kind = kind
        self.primcall_nargs = primcall_nargs
        self.primcall_code = primcall_code
        self.local_frame_idx = local_frame_idx
        self.local_var_idx = local_var_idx
        self.immutable = immutable


class EnvironmentFrame:
    def __init__(self, initial_variables=None):
        if initial_variables is None:
            self.variables = []
        else:
            self.variables = initial_variables

        self.macros = {}

    def copy(self):
        copy = EnvironmentFrame()
        copy.variables = [s for s in self.variables]
        copy.macros = {
            name: unique_name
            for name, unique_name in self.macros.items()
        }
        return copy

    def contains(self, name: Symbol):
        return name in self.variables

    def add_variable(self, name: Symbol):
        self.variables.append(name)

    def add_macro(self, name: Symbol):
        if name in self.macros:
            raise CompileError(
                f'Duplicate macro definition: {name}')
        self.macros[name] = Symbol.gensym()

    def find_macro(self, name: Symbol):
        return self.macros.get(name, None)


class Environment:
    def __init__(self, *, primcalls_enabled=True):
        self.frames: list[EnvironmentFrame] = []
        self.defined_symbols: dict[Symbol, DefineInfo] = {}
        self.toplevel_macros = []
        self.primcalls_enabled = primcalls_enabled

    def copy(self):
        copy = Environment()
        copy.frames = [f.copy() for f in self.frames]
        copy.toplevel_macros = [m for m in self.toplevel_macros]
        return copy

    def add_frame(self, variables):
        self.frames.insert(0, EnvironmentFrame(variables))

    def locate_local(self, name: Symbol):
        for i, frame in enumerate(self.frames):
            try:
                j = frame.variables.index(name)
            except ValueError:
                pass
            else:
                return [Integer(i), Integer(j)]
        return None

    def add_macro(self, name: Symbol):
        self.toplevel_macros.append(name)
        self.defined_symbols[name] = DefineInfo(is_macro=True)

    def find_macro(self, name: Symbol):
        """
        Look up a name as a macro. If found as a local macro, its unique
        global name is returned. If found as a top-level macro, its name is
        returned (which is the same as the defined name). Otherwise None is
        returned.
        """

        if name in self.toplevel_macros:
            return name

        for frame in self.frames:
            unique_name = frame.find_macro(name)
            if unique_name is not None:
                return unique_name

        return None

    def is_exported_primcall(self, name: str) -> bool:
        if not self.primcalls_enabled:
            return False

        return primcalls.get(name, {}).get('exported', False)

    def lookup_symbol(self, sym: Symbol) -> SymbolInfo:
        if sym.name.startswith('#$/'):
            m = re.match(r'#\$/(?P<module>\w+)/(?P<proc>\w+)', sym.name)
            if not m:
                raise CompileError(
                    f'Unknown runtime procedure: {sym}',    form=sym)

            module = m.group('module')
            proc = m.group('proc')
            desc = runtime.find_proc(module, proc)
            if desc is None:
                raise CompileError(
                    f'Unknown runtime procedure: {sym.name}',
                    form=sym)

            return SymbolInfo(
                symbol=sym,
                kind=SymbolKind.PRIMCALL,
                primcall_nargs=len(desc['args']),
                primcall_code=[S('trap'), S(f'{module}/{proc}')],
                immutable=True,
            )
        elif sym.name.startswith('#$'):
            # a symbol with a #$ prefix always refers to a primcall in any
            # environment.
            actual_name = sym.name[2:]
            primcall_info = primcalls.get(actual_name)
            if primcall_info is None:
                raise CompileError(
                    f'No such primitive: {actual_name}', form=sym)
            return SymbolInfo(
                symbol=sym,
                kind=SymbolKind.PRIMCALL,
                primcall_nargs=primcall_info['nargs'],
                primcall_code=primcall_info['code'],
                immutable=True,
            )
        elif self.is_exported_primcall(sym.name):
            primcall_info = primcalls[sym.name]
            return SymbolInfo(
                symbol=sym,
                kind=SymbolKind.PRIMCALL,
                primcall_nargs=primcall_info['nargs'],
                primcall_code=primcall_info['code'],
                immutable=True,
            )
        elif local_info := self.locate_local(sym):
            return SymbolInfo(
                symbol=sym,
                kind=SymbolKind.LOCAL,
                local_frame_idx=local_info[0],
                local_var_idx=local_info[1],
            )
        else:
            return SymbolInfo(
                symbol=sym,
                kind=SymbolKind.FREE,
            )


primcalls = {
    'apply': {
        'nargs': 2,
        'code': [S('ap')],
        'exported': False,
    },
    'void': {
        'nargs': 0,
        'code': [S('void')],
        'exported': False,
    },
    'call/cc': {
        'nargs': 1,
        'code': [S('ccc')],
        'exported': False,
    },
    'values->list': {
        'nargs': 1,
        'code': [S('m2l')],
        'exported': False,
    },
    'list->values': {
        'nargs': 1,
        'code': [S('l2m')],
        'exported': False,
    },
    'iadd': {
        'nargs': 2,
        'code': [S('iadd')],
        'exported': False,
    },
    'isub': {
        'nargs': 2,
        'code': [S('isub')],
        'exported': False,
    },
    'imul': {
        'nargs': 2,
        'code': [S('imul')],
        'exported': False,
    },
    'idiv': {
        'nargs': 2,
        'code': [S('idiv')],
        'exported': False,
    },
    'irem': {
        'nargs': 2,
        'code': [S('irem')],
        'exported': False,
    },
    'ilt': {
        'nargs': 2,
        'code': [S('ilt')],
        'exported': False,
    },
    'ile': {
        'nargs': 2,
        'code': [S('ile')],
        'exported': False,
    },
    'shr': {
        'nargs': 2,
        'code': [S('shr')],
        'exported': False,
    },
    'shl': {
        'nargs': 2,
        'code': [S('shl')],
        'exported': False,
    },
    'asr': {
        'nargs': 2,
        'code': [S('asr')],
        'exported': False,
    },
    'bnot': {
        'nargs': 1,
        'code': [S('bnot')],
        'exported': False,
    },
    'band': {
        'nargs': 2,
        'code': [S('band')],
        'exported': False,
    },
    'bor': {
        'nargs': 2,
        'code': [S('bor')],
        'exported': False,
    },
    'bxor': {
        'nargs': 2,
        'code': [S('bxor')],
        'exported': False,
    },
    'cons': {
        'nargs': 2,
        'code': [S('cons')],
        'exported': True,
    },
    'car': {
        'nargs': 1,
        'code': [S('car')],
        'exported': True,
    },
    'cdr': {
        'nargs': 1,
        'code': [S('cdr')],
        'exported': True,
    },
    'set-car!': {
        'nargs': 2,
        'code': [S('setcar'), S('void')],
        'exported': True,
    },
    'set-cdr!': {
        'nargs': 2,
        'code': [S('setcdr'), S('void')],
        'exported': True,
    },
    'type': {
        'nargs': 1,
        'code': [S('type')],
        'exported': False,
    },
    'eq?': {
        'nargs': 2,
        'code': [S('eq')],
        'exported': True,
    },
    'gensym': {
        'nargs': 1,
        'code': [S('gensym')],
        'exported': False,
    },
    'char->integer': {
        'nargs': 1,
        'code': [S('ch2i')],
        'exported': True,
    },
    'integer->char': {
        'nargs': 1,
        'code': [S('i2ch')],
        'exported': True,
    },
    'char-general-category': {
        'nargs': 1,
        'code': [S('ugcat')],
        'exported': True,
    },
    'char-upcase': {
        'nargs': 1,
        'code': [S('chup')],
        'exported': True,
    },
    'char-downcase': {
        'nargs': 1,
        'code': [S('chdn')],
        'exported': True,
    },
    'char-foldcase': {
        'nargs': 1,
        'code': [S('chfd')],
        'exported': True,
    },
    'digit-value': {
        'nargs': 1,
        'code': [S('chdv')],
        'exported': True,
    },
    'make-string': {
        'nargs': 2,
        'code': [S('mkstr')],
        'exported': False,
    },
    'string-ref': {
        'nargs': 2,
        'code': [S('strref')],
        'exported': True,
    },
    'string-set!': {
        'nargs': 3,
        'code': [S('strset'), S('void')],
        'exported': True,
    },
    'string-length': {
        'nargs': 1,
        'code': [S('strlen')],
        'exported': True,
    },
    'symbol->string': {
        'nargs': 1,
        'code': [S('sym2str')],
        'exported': True,
    },
    'string->symbol': {
        'nargs': 1,
        'code': [S('str2sym')],
        'exported': True,
    },
    'make-vector': {
        'nargs': 2,
        'code': [S('mkvec')],
        'exported': False,
    },
    'vector-ref': {
        'nargs': 2,
        'code': [S('vecref')],
        'exported': True,
    },
    'vector-set!': {
        'nargs': 3,
        'code': [S('vecset'), S('void')],
        'exported': False,
    },
    'vector-length': {
        'nargs': 1,
        'code': [S('veclen')],
        'exported': True,
    },
    'wrap': {
        'nargs': 2,
        'code': [S('wrap')],
        'exported': False,
    },
    'unwrap': {
        'nargs': 1,
        'code': [S('unwrap')],
        'exported': False,
    },
    'set-system-exception-handler': {
        'nargs': 1,
        'code': [S('seh'), S('void')],
        'exported': False,
    },
    'abort': {
        'nargs': 2,
        'code': [S('abort')],
        'exported': False,
    },
    'features': {
        'nargs': 0,
        'code': asm_code_for_features(),
        'exported': True,
    },
}


class CompileError(Exception):
    def __init__(self, msg, form=None, source=None):
        self.msg = msg
        self.form = form
        self.source = source

    def __repr__(self):
        return self.msg

    def print_snippet(self):
        if self.form is None or self.source is None:
            return

        if self.form.src_start is None or self.form.src_end is None:
            return

        print()
        if self.source.filename:
            print(f'--- source file: {self.source.filename}')
        show_snippet(
            self.source.text, self.form.src_start, self.form.src_end,
            pre_lines=3, post_lines=3)


class Compiler:
    def __init__(self, libs: list[Fasl], debug_info=False):
        self.assembler = Assembler()
        self.set_symbols = set()
        self.read_symbols = set()
        self.defines_fasl = Fasl()
        self.macros_fasl = Fasl()
        self.libs = libs
        self.debug_info = debug_info
        self.include_paths = []

        self.current_source = None
        self.current_form = None

        # for now, we'll treat everything as a library
        self.compiling_library = True

    def _compile_error(self, msg, form=None, source=None):
        form = form or self.current_form
        source = source or self.current_source
        return CompileError(msg, form=form, source=source)

    def _rebuild_compile_error(self, e: CompileError) -> CompileError:
        # re-create a CompileError exception, possibly adding current debug info
        # ("form" and "source") to it, if they're missing.
        return self._compile_error(
            msg=str(e),
            form=e.form,
            source=e.source)

    def lookup_symbol(self, sym: Symbol, env: Environment) -> SymbolInfo:
        try:
            return env.lookup_symbol(sym)
        except CompileError as e:
            raise self._rebuild_compile_error(e)

    def is_macro(self, name: Symbol, env: Environment):
        if env.find_macro(name):
            return True

        for lib in self.libs:
            for sym, info in lib.defines.items():
                if sym == name and info.is_macro:
                    return True

        return False

    def macro_expand(self, form, env):
        while isinstance(form, Pair) and \
              len(form) > 0 and \
              isinstance(form[0], Symbol):
            name_sym = form[0]
            if not self.is_macro(name_sym, env):
                break

            src_start = form.src_start
            src_end = form.src_end

            args = form.cdr
            form = self.expand_single_macro(name_sym, args, env, form)
            if isinstance(form, Pair) and not form.is_proper():
                raise self._compile_error(
                    f'Macro {name_sym} returned an improper list: {form}')

            if isinstance(form, Pair) and not form.is_proper():
                raise self._compile_error(
                    f'Macro {name_sym} returned an improper list: '
                    f'{form}')

            form.src_start = src_start
            form.src_end = src_end

        return form

    def macro_expand_full(self, form, env):
        while isinstance(form, Pair) and \
              len(form) > 0 and \
              isinstance(form[0], Symbol):
            name_sym = form[0]
            if name_sym.name == 'quote':
                return form

            if not self.is_macro(name_sym, env):
                break

            args = form.cdr
            form = self.expand_single_macro(name_sym, args, env, form)

        if not isinstance(form, Pair):
            return form

        rest = form.cdr
        while rest != Nil():
            if isinstance(rest.car, Pair):
                rest.car = self.macro_expand_full(rest.car, env)
            rest = rest.cdr

        return form

    def expand_single_macro(self, name_sym, args, env, form):
        unique_name = env.find_macro(name_sym)
        if unique_name is None:
            # must be a library macro, which should be under its own name
            unique_name = name_sym

        fasl = Fasl()
        func_call_code = [S('get'), unique_name, S('ap')]
        self.assembler.assemble(func_call_code, fasl)

        machine = Secd()

        try:
            # since we want to push arguments on the stack, load the libraries
            # first to make sure they won't interfere with what we push.
            machine.load_fasls(self.libs + [self.defines_fasl, self.macros_fasl])
        except RunError as e:
            raise self._compile_error(
                f'Run error when loading libs for macro expansion of '
                f'"{name_sym}": {e}')

        # push the arguments directly on the machine stack. this is a better
        # approach than generating code for the quoted forms of the arguments.
        # not only it requires less code, but also we're passing the exact same
        # code objects/lists we have here in the compiler, and if they are
        # incorporated in the macro output, they still have any source
        # locations/debug info associated with them.
        machine.s.push(args)

        try:
            machine.execute_fasl(fasl)
        except RunError as e:
            raise self._compile_error(
                f'Run error during macro expansion of "{name_sym}": {e}')

        if len(machine.s) == 0:
            raise self._compile_error(
                f'Internal error: macro did not return anything')

        expanded = machine.s.top()
        return expanded

    def parse_define_form(self, expr, form_name):
        if len(expr) < 2:
            raise self._compile_error(f'Invalid number of arguments for {form_name}.')

        if isinstance(expr[1], Symbol):
            name = expr[1]
            if len(expr) == 3:
                value = expr[2]
            elif len(expr) == 2:
                # define with no value
                # convert from: (define name)
                # to: (define name (#$void))
                # which is: (define name . ( #$void . () ) )
                value = Pair(Symbol('#$void'), Nil())
            else:
                raise self._compile_error(
                    f'Invalid number of arguments for {form_name}.')
        elif isinstance(expr[1], Pair):
            if len(expr) < 3:  # (define (foo))
                raise self._compile_error(
                    f'Invalid number of arguments for {form_name}.')
            if expr[1] == Nil():  # (define () x)
                raise self._compile_error(f'Malformed {form_name}.')

            # expr: (define . ((name . params) . body))
            # expr[1] is in this form: (name . params)
            name = expr.cdar().car
            params = expr.cdar().cdr
            body = expr.cddr()

            if not isinstance(name, Symbol):
                raise self._compile_error(
                    f'Invalid name for {form_name}: {name}')

            # form: (lambda . ( params . body ))
            value = Pair(S('lambda'), Pair(params, body))
        else:
            raise self._compile_error(f'Malformed {form_name}.')

        value.src_start = expr.src_start
        value.src_end = expr.src_end

        return name, value

    def process_define_macro(self, expr, env):
        # we'll be compiling the macro almost exactly the same as we compile a
        # regular top-level define. we only set the define type as a macro in
        # env.defined_symbols.

        name, lambda_form = self.parse_define_form(expr, 'define-macro')
        if len(expr) < 3:
            raise self._compile_error('Not enough arguments for define-macro')

        if env.find_macro(name):
            raise self._compile_error(
                f'Duplicate macro definition: {name}')
        try:
            env.add_macro(name)
        except CompileError as e:
            # re-raise compile error with appropriate debug info attached
            self._rebuild_compile_error(e)

        code = self.compile_form(lambda_form, env)
        code += [S('set'), name, S('void')]

        return code

    def compile_int(self, expr, env):
        return [S('ldc'), Integer(expr)]


    def compile_if(self, expr, env):
        if len(expr) not in (3, 4):
            raise self._compile_error(
                f'Invalid number of arguments for if: {expr}')

        cond_code = self.compile_form(expr[1], env)
        true_code = self.compile_form(expr[2], env) + [S('join')]
        if len(expr) == 4:
            false_code = self.compile_form(expr[3], env) + [S('join')]
        else:
            false_code = [S('void'), S('join')]
        return cond_code + [S('sel')] + [true_code] + [false_code]

    def collect_defines(self, body):
        # receive the body a lambda (or let and the like).
        # the body forms should have been macro-expanded already (not recursively)
        # collects all initial defines (including those in begin statements)
        # returns a pair, consisting of the list of defines, as well as the rest of the body

        defines = []
        while not isinstance(body, Nil):
            form = body.car
            if not isinstance(form, Pair):
                break

            if form.car == S('define'):
                defines.append(('define', form))
            elif form.car == S('define-macro'):
                defines.append(('define-macro', form))
            elif form.car == S('begin'):
                begin_defines, begin_rest = self.collect_defines(form.cdr)
                defines += begin_defines
                if begin_rest != Nil():
                    # add whatever is remaining in the begin body to the
                    # beginngin of the main body.
                    body = body.cdr # remove current "begin" statement from the body
                    for i in reversed(begin_rest.to_list()):
                        body = Pair(i, body)
                    break
            else:
                break

            body = body.cdr

        return defines, body

    def compile_body(self, body, env, full_form):
        code = []

        # first macro expand the body, since collect_defines expects that.
        cur = body
        while not isinstance(cur, Nil):
            cur.car = self.macro_expand(cur.car, env)
            cur = cur.cdr

        defines, body = self.collect_defines(body)

        # expand the environment to include the defines
        seen = set()
        for define_type, define_form in defines:
            # a name cannot be defined more than once
            name, value = self.parse_define_form(define_form, define_type)
            if name in seen:
                raise self._compile_error(
                    f'Duplicate definition: {name}', form=define_form)
            seen.add(name)

            if define_type == 'define':
                # it's okay if the name already exists though, we're just
                # shadowing a let/letrec/lambda varaible
                if not env.frames[0].contains(name):
                    env.frames[0].add_variable(name)
                    code += [S('xp')]
            elif define_type == 'define-macro':
                try:
                    env.frames[0].add_macro(name)
                except CompileError as e:
                    # re-raise compile error with appropriate debug info attached
                    raise self._rebuild_compile_error(e)
            else:
                assert False, 'Unhandled define type'

        # now that the environment has all the new variables, set variable
        # values
        for define_type, define_form in defines:
            name, value = self.parse_define_form(define_form, define_type)
            if define_type == 'define':
                code += self.compile_form(value, env)
                code += [S('st'), env.locate_local(name)]
            elif define_type == 'define-macro':
                # compile the macro as a global define under its unique name in
                # the macros fasl.
                unique_name = env.find_macro(name)
                macro_code = self.compile_form(value, env)
                macro_code += [S('set'), unique_name]
                self.assembler.assemble(macro_code, self.macros_fasl)
            else:
                assert False, 'Unhandled define type'

        if isinstance(body, Nil):
            raise self._compile_error('Empty body', form=full_form)

        # compile the rest of the body normally
        for i, e in enumerate(body):
            code += self.compile_form(e, env)
            if i < len(body) - 1:
                code.append(S('drop'))

        return code

    def compile_lambda(self, expr, env):
        if len(expr) < 3:
            raise self._compile_error(
                f'Invalid number of arguments for lambda: {expr}')

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

        seen = set()
        for p in params:
            if p in seen:
                raise self._compile_error(
                    f'Duplicate variable: {p}', form=params)
            seen.add(p)
            if not isinstance(p, Symbol):
                raise self._compile_error(
                    f'Invalid parameter name: {p} (not a symbol)',
                    form=p)

        if rest_param and not isinstance(rest_param, Symbol):
            raise self._compile_error(
                f'Invalid parameter name: {rest_param}',
                form=rest_param)

        if rest_param:
            params = params + [rest_param]

        # we store the number of parameters here, becaues when we compile the
        # body further down, the params value might change in case there are
        # "define" forms in the body. we will use this original value when
        # writing the "ldf" instruction though.
        original_nparams = len(params)

        new_env = env.copy()
        new_env.add_frame(params)

        # expr: (lambda . (params . body))
        body = expr.cddr()
        body_code = self.compile_body(body, new_env, full_form=expr)
        body_code += [S('ret')]
        if body_code[-2] == S('ap') or body_code[-2] == S('ap'):
            body_code[-2:] = [S('tap')]

        if rest_param:
            code = [S('ldf'), Integer(-original_nparams), body_code]
        else:
            code = [S('ldf'), Integer(original_nparams), body_code]

        return code

    def compile_symbol(self, sym: Symbol, env):
        if self.is_macro(sym, env):
            raise self._compile_error(
                f'Invalid use of macro name: {sym}',
                form=sym)

        info = self.lookup_symbol(sym, env)
        if info.kind == SymbolKind.PRIMCALL:
            # using a primitive like "car" or "cons" as a symbol. this should
            # return a function that performs those primitives. what we do is
            # emit an ldf instruction which loads its arguments onto the stack,
            # in the same order as the call to the primitive itself pushes its
            # arguments, then performs the same instructions the primitive
            # itself would perform, and then returns of course.
            func_code = []
            for i in range(info.primcall_nargs - 1, -1, -1):
                func_code += [S('ld'), [Integer(0), Integer(i)]]
            func_code += info.primcall_code
            func_code += [S('ret')]
            return [S('ldf'), Integer(info.primcall_nargs), func_code]
        elif info.kind == SymbolKind.LOCAL:
            return [S('ld'), [info.local_frame_idx, info.local_var_idx]]
        elif info.kind == SymbolKind.FREE:
            self.read_symbols.add((sym, self.current_source))
            return [S('get'), sym]
        else:
            assert False, 'unhandled symbol kind'

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
            raise self._compile_error(
                f'Invalid bindings list for {let_name}: {bindings}',
                form=bindings)

        if not bindings.is_proper():
            raise self._compile_error(
                f'Invalid {let_name} bindings: {bindings}',
                form=bindings)

        for pair in bindings:
            if not isinstance(pair, Pair) or len(pair) != 2 or not isinstance(pair[0], Symbol):
                raise self._compile_error(
                    f'Invalid {let_name} binding: {pair}',
                    form=bindings)

    def compile_let(self, expr, env):
        if len(expr) < 2:
            raise self._compile_error(
                f'Invalid number of arguments for let: {expr}')

        # convert named let to letrec
        if isinstance(expr[1], Symbol):
            if len(expr) < 3:
                raise self._compile_error(
                    f'Invalid number of arguments for named-let: {expr}')

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
                raise self._compile_error(f'Invalid let variable: {v}', form=v)

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
            raise self._compile_error(
                f'Invalid number of arguments for letrec: {expr}')

        bindings = expr[1]
        self.check_let_bindings(expr[1], 'letrec')

        # bindings (( a . (value1 . nil) (b . (value2 . nil))))
        vars = List.from_list([b.car for b in bindings])
        values = List.from_list([b.cdar() for b in bindings])
        body = expr.cddr()

        for v in vars:
            if not isinstance(v, Symbol):
                raise self._compile_error(f'Invalid let variable: {v}', form=v)

        if values != Nil():
            for v, b in zip(values, bindings):
                v.src_start = b.cdar().src_start
                v.src_end = b.cdar().src_end

        secd_code = [S('dum'), S('nil')]
        for v in reversed(values.to_list()):
            new_env = env.copy()
            new_env.add_frame(vars)
            secd_code += self.compile_form(v, new_env) + [S('cons')]

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

    def compile_set(self, expr, env):
        if len(expr) != 3:
            raise self._compile_error(
                f'Invalid number of arguments for set!')

        if not isinstance(expr[1], Symbol):
            raise self._compile_error(
                f'Variable name passed to set! not a symbol')

        name = expr[1]
        value = expr[2]
        code = self.compile_form(value, env)

        info = self.lookup_symbol(name, env)
        if info.immutable:
            raise self._compile_error(
                f'Attempting to assign immutable variable: {name}',
                form=name)

        if info.kind == SymbolKind.LOCAL:
            code += [S('st'), [info.local_frame_idx, info.local_var_idx]]
        elif info.kind == SymbolKind.FREE:
            code += [S('set'), name]
            self.set_symbols.add((name, self.current_source))
        else:
            assert False, 'unhandled symbol kind'

        code += [S('void')]

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
            raise self._compile_error(
                f'Internal error: do not know how to create object of '
                f'type: {type(form).__name__}')

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
            raise self._compile_error(
                f'Invalid number of arguments for quote.')

        return self.compile_literal(expr[1], env)

    def compile_primcall(self, expr, env, info: SymbolInfo):
        if len(expr) != info.primcall_nargs + 1:
            raise self._compile_error(
                f'Invalid number of arguments for "{expr[0]}" '
                f'(expected: {info.primcall_nargs}, got {len(expr)-1})')

        code = []
        for i in range(info.primcall_nargs - 1, -1, -1):
            code += self.compile_form(expr[i + 1], env)

        code += info.primcall_code
        return code

    def compile_begin(self, expr, env):
        if expr.cdr == Nil():
            # unlike at the top-level, or at the beginning of a lambda/let body,
            # the normal "begin" expression cannot be empty.
            raise self._compile_error('Empty "begin" expression')

        code = []
        for i, form in enumerate(expr.cdr):
            if i > 0:
                code += [S('drop')]
            code += self.compile_form(form, env)
        return code

    def find_include_file(self, filename: str):
        if os.path.exists(filename):
            return filename
        else:
            for path in self.include_paths:
                full_path = os.path.join(path, filename)
                if os.path.exists(full_path):
                    return str(full_path)

        return None

    def _compile_include(self, expr, env, *, toplevel: bool, casefold: bool):
        if len(expr) < 2:
            raise self._compile_error(
                'Missing filenames in include directive')

        code = []
        for filename in expr.cdr:
            if not isinstance(filename, String):
                raise self._compile_error(
                    f'Filename in include directive not a string '
                    f'literal: {filename}')

            filename = filename.value
            full_path = self.find_include_file(filename)
            if full_path is None:
                raise self._compile_error(
                    f'Included file not found: {filename}')

            with open(full_path) as f:
                try:
                    reader = Reader(f, casefold=casefold)
                    exprs = reader.read_all()
                except ReadError as e:
                    raise self._compile_error(
                        f'Read error while expanding included file '
                        f'"{filename}": {e}')

            if exprs == []:
                continue

            exprs = List.from_list(exprs)
            begin_form = Pair(S('begin'), exprs)

            if toplevel:
                old_source = self.current_source
                self.current_source = SourceFile(filename=full_path)
                include_code = self.compile_toplevel_form(begin_form, env)
                self.current_source = old_source
            else:
                include_code = self.compile_form(begin_form, env)

            if self.debug_info:
                include_code = [S(':filename-start'), String(filename)] + include_code
                include_code += [S(':filename-end')]

            code += include_code

        return code

    def compile_include_toplevel(self, expr, env):
        return self._compile_include(
            expr, env,
            toplevel=True,
            casefold=False)

    def compile_include_ci_toplevel(self, expr, env):
        return self._compile_include(
            expr, env,
            toplevel=True,
            casefold=True)

    def compile_include_local(self, expr, env):
        return self._compile_include(
            expr, env,
            toplevel=True,
            casefold=False)

    def compile_include_ci_local(self, expr, env):
        return self._compile_include(
            expr, env,
            toplevel=False,
            casefold=True)

    def _compile_cond_expand(self, expr, env, *, toplevel: bool):
        if len(expr) < 2:
            raise self._compile_error(
                'Invalid number of arguments for cond-expand')

        features = get_features()
        def match(req):
            if isinstance(req, Symbol):
                return req.name in features
            elif isinstance(req, Pair):
                if req.car == S('and'):
                    return all(match(r) for r in req.cdr)
                elif req.car == S('or'):
                    return any(match(r) for r in req.cdr)
                elif req.car == S('not'):
                    return not(match(req.cdr))
                elif req.car == S('library'):
                    raise self._compile_error(
                        'cond-expand library clause not implemented yet',
                        form=req)
            return False

        code = []
        for clause in expr.cdr:
            if not isinstance(clause, Pair) or len(clause) < 2:
                raise self._compile_error(
                    f'Invalid cond-expand clause: {clause}',
                    form=clause)
            requirement = clause.car
            expressions = clause.cdr
            if match(requirement):
                begin_form = Pair(S('begin'), expressions)
                begin_form.src_start = clause.src_start
                begin_form.src_end = clause.src_end
                if toplevel:
                    code += self.compile_toplevel_form(begin_form, env)
                else:
                    code += self.compile_form(begin_form, env)

        return code

    def compile_cond_expand_toplevel(self, expr, env):
        return self._compile_cond_expand(expr, env, toplevel=True)

    def compile_cond_expand_local(self, expr, env):
        return self._compile_cond_expand(expr, env, toplevel=False)

    def compile_list(self, expr, env):
        if expr == Nil():
            raise self._compile_error(
                'Empty list is not a valid form')

        if not expr.is_proper():
            raise self._compile_error(
                f'Cannot compile improper list: {expr}')

        if isinstance(expr.car, Symbol):
            sym = expr.car
            name = sym.name
            special_forms = {
                'begin': self.compile_begin,
                'set!': self.compile_set,
                'if': self.compile_if,
                'lambda': self.compile_lambda,
                'let': self.compile_let,
                'letrec': self.compile_letrec,
                'quote': self.compile_quote,
                'include': self.compile_include_local,
                'include-ci': self.compile_include_ci_local,
                'cond-expand': self.compile_cond_expand_local,
            }

            compile_func = special_forms.get(name)
            if name in special_forms:
                compile_func = special_forms[name]
                return compile_func(expr, env)
            else:
                info = self.lookup_symbol(sym, env)
                if info.kind == SymbolKind.PRIMCALL:
                    return self.compile_primcall(expr, env, info)
                else:
                    return self.compile_func_call(expr, env)
        else:
            return self.compile_func_call(expr, env)

    def _compile_form(self, expr, env):
        if self.detect_cycle(expr):
            raise self._compile_error(
                f'Cannot compile cyclic list: {expr}')
        if isinstance(expr, Pair) and not expr.is_proper():
            raise self._compile_error(
                f'Cannot compile improper list: {expr}')

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
            raise self._compile_error(f'Invalid value: {expr}')

        if self.debug_info and expr.src_start is not None and expr.src_end is not None:
            secd_code += [S(':expr-end'), Integer(expr.src_end)]

        return secd_code

    def compile_form(self, expr, env):
        old_current_form = self.current_form
        self.current_form = expr
        try:
            return self._compile_form(expr, env)
        finally:
            self.current_form = old_current_form

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

    def _compile_toplevel_form(self, form, env):
        if isinstance(form, Pair) and not form.is_proper():
            raise self._compile_error(
                f'Cannot compile improper list: {form}')

        form = self.macro_expand(form, env)
        if not isinstance(form, Pair):
            return self.compile_form(form, env)

        if form[0] == S('define-macro'):
            form_code = self.process_define_macro(form, env)
            if isinstance(form[1], Symbol):
                defined_sym = form[1]     # (define-macro foo value)
            else:
                defined_sym = form[1][0]  # (define-macro (foo . formals) . body)

            self.macros_fasl.add_define(defined_sym, is_macro=True)
            self.assembler.assemble(form_code, self.macros_fasl)

            # if we're not compiling a library, do not include the macro
            # code in the output.
            if not self.compiling_library:
                form_code = []
        elif form[0] == S('define'):
            name_sym, value = self.parse_define_form(form, 'define')
            env.defined_symbols[name_sym] = DefineInfo(is_macro=False)
            form_code = self.compile_form(value, env)
            form_code += [S('set'), name_sym, S('void')]

            if self.debug_info:
                if form.src_start is not None and form.src_end is not None:
                    form_code = \
                        [S(':define-start'), String(name_sym.name), Integer(form.src_start)] + \
                        form_code + \
                        [S(':define-end'), Integer(form.src_end)]
            self.defines_fasl.add_define(name_sym, is_macro=False)
            self.assembler.assemble(form_code, self.defines_fasl)
        elif form[0] == S('begin'):
            form_code = []
            for i, expr in enumerate(form.cdr):
                form_code += self.compile_toplevel_form(expr, env)
                if i > 0:
                    form_code += [S('drop')]
            if form_code == []:
                form_code = [S('void')]
        elif form[0] == S('include'):
            form_code = self.compile_include_toplevel(form, env)
        elif form[0] == S('include-ci'):
            form_code = self.compile_include_ci_toplevel(form, env)
        elif form[0] == S('cond-expand'):
            form_code = self.compile_cond_expand_toplevel(form, env)
        else:
            form_code = self.compile_form(form, env)

        return form_code

    def compile_toplevel_form(self, form, env):
        old_current_form = self.current_form
        self.current_form = form
        try:
            return self._compile_toplevel_form(form, env)
        finally:
            self.current_form = old_current_form

    def compile_toplevel(self, text, env=None, *, filename=None):
        if env is None:
            env = Environment()

        code = []
        self.current_source = SourceFile(text=text, filename=filename)
        input = io.StringIO(text)
        reader = Reader(input)
        while True:
            try:
                form = reader.read()
            except ReadError as e:
                raise self._compile_error(f'Read error: {e}')
            if form is None:  # eof
                break

            if self.detect_cycle(form):
                raise self._compile_error(
                    f'Cannot compile cyclic form: {form}', form=form)
            if isinstance(form, Pair) and not form.is_proper():
                raise self._compile_error(
                    f'Cannot compile improper list: {form}', form=form)

            form_code = self.compile_toplevel_form(form, env)

            if code == []:
                code = form_code
            elif form_code != []:
                code += [S('drop')] + form_code

        if code != []:
            code += [S('drop')]

        if filename and self.debug_info:
            code = [S(':filename-start'), String(filename)] + code
            code += [S(':filename-end')]

        all_defines = set(env.defined_symbols.keys())
        for lib in self.libs:
            all_defines |= set(lib.defines)

        for sym, filename in self.set_symbols:
            if sym not in all_defines:
                raise self._compile_error(
                    f'Symbol {sym} is set at some point but never defined',
                    form=sym, source=filename)

        for sym, filename in self.read_symbols:
            if sym not in all_defines:
                raise self._compile_error(
                    f'Symbol {sym} is read at some point but never defined',
                    form=sym, source=filename)

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
        source_filename = None
    else:
        source_filename = args.input
        with open(args.input) as f:
            text = f.read()

    try:
        secd_code = compiler.compile_toplevel(text, filename=source_filename)
    except ReadError as e:
        print(f'Read error: {e}', file=sys.stderr)
        sys.exit(1)
    except CompileError as e:
        print(f'Compile error: {e}', file=sys.stderr)
        e.print_snippet(filename=args.input)
        sys.exit(1)

    secd_code = List.from_list_recursive(secd_code)
    print(secd_code)
