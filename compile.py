#!/usr/bin/env python3

from enum import Enum
import io
import os
from pathlib import Path
import re
import platform
import sys
import argparse
from library import LibraryExportedSymbol, LibraryName
from program import Program

import runtime
from fasl import DefineInfo, Fasl
from read import Reader, ReadError
from machinetypes import Bool, Char, Integer, List, Nil, Pair, Symbol, String, Vector
from assemble import Assembler
from secd import RunError, Secd
from snippet import show_snippet
from utils import OrderedSet, find_shared
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


class SymbolKind(Enum):
    SPECIAL = 1
    PRIMCALL = 2
    LOCAL = 3
    LIBRARY = 4
    FREE = 5
    MACRO = 6
    AUX = 7


class AuxKeywords(Enum):
    UNQUOTE = 'unquote'
    UNQUOTE_SPLICING = 'unquote-splicing'
    ELSE = 'else'
    ARROW = '=>'
    UNDERSCORE = '_'
    ELLIPSIS = '...'


class SpecialForms(Enum):
    DEFINE = 'define'
    DEFINE_MACRO = 'define-macro'
    DEFINE_LIBRARY = 'define-library'
    BEGIN = 'begin'
    SET = 'set!'
    IF = 'if'
    LAMBDA = 'lambda'
    LET = 'let'
    LETREC = 'letrec'
    QUOTE = 'quote'
    INCLUDE = 'include'
    INCLUDE_CI = 'include-ci'
    COND_EXPAND = 'cond-expand'


class SymbolInfo:
    def __init__(self, symbol: Symbol, kind: SymbolKind, *,
                 primcall_nargs=None,
                 primcall_code=None,
                 local_frame_idx=None,
                 local_var_idx=None,
                 special_type=None,
                 aux_type=None,
                 library_name=None,
                 immutable=False):
        self.symbol = symbol
        self.kind = kind
        self.primcall_nargs = primcall_nargs
        self.primcall_code = primcall_code
        self.local_frame_idx = local_frame_idx
        self.local_var_idx = local_var_idx
        self.special_type = special_type
        self.aux_type = aux_type
        self.library_name = library_name
        self.immutable = immutable

    def is_special(self, special_type: SpecialForms) -> bool:
        return self.kind == SymbolKind.SPECIAL and \
            self.special_type == special_type

    def is_aux(self, aux_type: AuxKeywords) -> bool:
        return self.kind == SymbolKind.AUX and \
            self.aux_type == aux_type

    def __repr__(self):
        return f'<SymbolInfo {self.symbol} kind={self.kind.name}>'


class ImportSet:
    def lookup(self, sym: Symbol) -> (SymbolInfo | None):
        raise NotImplementedError


class CoreImportSet(ImportSet):
    def lookup(self, sym: Symbol) -> (SymbolInfo | None):
        m = re.match(r'#\$/(?P<module>\w+)/(?P<proc>\w+)', sym.name)
        if m:
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

        if sym.name.startswith('#$'):
            desc = primcalls.get(sym.name[2:])
            found = bool(desc)
        else:
            desc = primcalls.get(sym.name)
            found = desc and desc['exported']
        if found:
            return SymbolInfo(
                symbol=sym,
                kind=SymbolKind.PRIMCALL,
                primcall_nargs=desc['nargs'],
                primcall_code=desc['code'],
                immutable=True,
            )

        if any(sym.name == i.value for i in SpecialForms):
            return SymbolInfo(
                symbol=sym,
                kind=SymbolKind.SPECIAL,
                special_type=SpecialForms(sym.name),
                immutable=True,
            )

        if any(sym.name == i.value for i in AuxKeywords):
            return SymbolInfo(
                symbol=sym,
                kind=SymbolKind.AUX,
                aux_type=AuxKeywords(sym.name),
                immutable=True,
            )

        return None

    def __str__(self):
        return '<CoreImportSet>'

    def __repr__(self):
        return str(self)

    def get_all_names(self):
        names = []
        names += [S(i.value) for i in SpecialForms]
        names += [S(i.value) for i in AuxKeywords]
        names += list(S(i) for i in primcalls.keys())
        return names


class LibraryImportSet(ImportSet):
    def __init__(self, lib_name: LibraryName, exports: list):
        self.lib_name = lib_name

        self.exports = []
        for export_info in exports:
            mangled = lib_name.mangle_symbol(export_info.internal)
            self.exports.append((mangled, export_info))

    def lookup(self, sym: Symbol) -> (SymbolInfo | None):
        for mangled_internal, export_info in self.exports:
            if sym == export_info.external:
                return SymbolInfo(
                    symbol=mangled_internal,
                    kind=SymbolKind.LIBRARY if not export_info.is_macro else SymbolKind.MACRO,
                    library_name=self.lib_name,
                    immutable=True,
                )

        return None

    def get_all_names(self):
        return [e.external for _, e in self.exports]

    def __str__(self):
        return f'<LibraryImportSet {self.lib_name}>'

    def __repr__(self):
        return str(self)

    @staticmethod
    def get_import_set(name: LibraryName, fasls: list[Fasl], *, local_libs=[]):
        if name.parts == [S('trick'), S('core')]:
            return CoreImportSet()
        else:
            for lib_name, lib_exports in local_libs:
                if lib_name == name:
                    return LibraryImportSet(lib_name, lib_exports)

            for fasl in fasls:
                lib_info = fasl.get_section('libinfo')
                if not lib_info:
                    continue
                for lib_name, exports in lib_info.libs.items():
                    if name == lib_name:
                        return LibraryImportSet(name, exports)

        return None


class OnlyImportSet(ImportSet):
    def __init__(self, base_import_set: ImportSet, identifiers: list[Symbol]):
        self.base_import_set = base_import_set
        self.identifiers = identifiers

    def lookup(self, sym: Symbol):
        if sym not in self.identifiers:
            return None

        return self.base_import_set.lookup(sym)

    def __str__(self):
        return f'<OnlyImportSet base={self.base_import_set} only={self.identifiers}>'

    def __repr__(self):
        return str(self)


class ExceptImportSet(ImportSet):
    def __init__(self, base_import_set: ImportSet, identifiers: list[Symbol]):
        self.base_import_set = base_import_set
        self.identifiers = identifiers

    def lookup(self, sym: Symbol):
        if sym in self.identifiers:
            return None

        return self.base_import_set.lookup(sym)

    def __str__(self):
        return f'<ExceptImportSet base={self.base_import_set} except={self.identifiers}>'

    def __repr__(self):
        return str(self)


class PrefixImportSet(ImportSet):
    def __init__(self, base_import_set: ImportSet, prefix: Symbol):
        self.base_import_set = base_import_set
        self.prefix = prefix.name

    def lookup(self, sym: Symbol):
        if not sym.name.startswith(self.prefix):
            return None

        no_prefix_name = S(sym.name[len(self.prefix):])
        result = self.base_import_set.lookup(no_prefix_name)
        if result is None:
            return None
        result.symbol = no_prefix_name
        return result

    def __str__(self):
        return f'<PrefixImportSet base={self.base_import_set} prefix="{self.prefix}">'

    def __repr__(self):
        return str(self)

class RenameImportSet(ImportSet):
    def __init__(self, base_import_set: ImportSet, renames: list[Pair]):
        self.base_import_set = base_import_set
        self.renames = renames

    def lookup(self, sym: Symbol):
        for rename in self.renames:
            from_name = rename[0]
            to_name = rename[1]
            if sym == to_name:
                result =  self.base_import_set.lookup(from_name)
                result.symbol = to_name
                return result
        return self.base_import_set.lookup(sym)

    def __str__(self):
        renames = [f'"{f}"=>"{t}"' for f, t in self.renames]
        renames = ' '.join(renames)
        return f'<RenameImportSet base={self.base_import_set} renames=({renames})>'

    def __repr__(self):
        return str(self)


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

    def __eq__(self, other):
        if not isinstance(other, SourceFile):
            return False
        return self._text == other._text and self._filename == other._filename

    def __hash__(self):
        return hash((self._text, self._filename))


class EnvironmentFrame:
    def __init__(self, initial_variables=None):
        if initial_variables is None:
            self.variables = []
        else:
            self.variables = initial_variables

    def copy(self):
        copy = EnvironmentFrame()
        copy.variables = [s for s in self.variables]
        return copy

    def contains(self, name: Symbol):
        return name in self.variables

    def add_variable(self, name: Symbol):
        self.variables.append(name)


class Environment:
    def __init__(self, *, lib_name=None):
        self.frames: list[EnvironmentFrame] = []
        self.macros = []
        self.import_sets = []
        self.exports = []
        self.lib_name = lib_name

    def copy(self):
        copy = Environment()
        copy.frames = [f.copy() for f in self.frames]
        copy.macros = [m for m in self.macros]
        copy.import_sets = self.import_sets
        copy.exports = [i for i in self.exports]
        copy.lib_name = self.lib_name
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
        self.macros.append(name)

    def add_import(self, import_set: ImportSet):
        self.import_sets.append(import_set)

    def lookup_symbol(self, sym: Symbol, *, at_head=False) -> SymbolInfo:
        local = self.locate_local(sym)
        if local:
            return SymbolInfo(
                symbol=sym,
                kind=SymbolKind.LOCAL,
                local_frame_idx=local[0],
                local_var_idx=local[1],
            )
        for import_set in self.import_sets:
            result = import_set.lookup(sym)
            if result is not None:
                if result.kind != SymbolKind.SPECIAL or at_head:
                    return result

        is_macro = sym in self.macros
        kind = SymbolKind.MACRO if is_macro else SymbolKind.FREE
        if self.lib_name:
            return SymbolInfo(
                symbol=self.lib_name.mangle_symbol(sym),
                kind=kind,
            )
        else:
            return SymbolInfo(
                symbol=sym,
                kind=kind,
            )

    def add_export(self, sym: Symbol, source_file: (SourceFile | None)):
        self.exports.append(
            LibraryExportedSymbol(
                sym, sym, export_source_file=source_file))

    def add_renamed_export(self,
                           internal: Symbol,
                           external: Symbol,
                           source_file: (SourceFile | None)):
        self.exports.append(
            LibraryExportedSymbol(
                internal, external, export_source_file=source_file))

    def get_all_names(self):
        names = []
        for frame in self.frames:
            names.extend(frame.variables)
        for import_set in self.import_sets:
            names.extend(import_set.get_all_names())
        return names


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
        'exported': True,
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
        self.set_symbols = OrderedSet()
        self.read_symbols = OrderedSet()
        self.macros_fasl = Fasl()
        self.lib_fasls = libs
        self.debug_info = debug_info
        self.include_paths = []
        self.defined_libs = []
        self.defined_symbols = {}

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

    def lookup_symbol(self, sym: Symbol, env: Environment, *, at_head=False) -> SymbolInfo:
        try:
            return env.lookup_symbol(sym, at_head=at_head)
        except CompileError as e:
            raise self._rebuild_compile_error(e)

    def is_macro(self, name: Symbol, env: Environment):
        if name in env.macros:
            return True

        for lib in self.lib_fasls:
            for sym, info in lib.defines.items():
                if sym == name and info.is_macro:
                    return True

        return False

    def macro_expand(self, form, env):
        while isinstance(form, Pair) and \
              len(form) > 0 and \
              isinstance(form[0], Symbol):
            name_sym = form[0]
            info = self.lookup_symbol(name_sym, env)
            if info.kind != SymbolKind.MACRO:
                break

            src_start = form.src_start
            src_end = form.src_end

            args = form.cdr
            form = self.expand_single_macro(name_sym, info, args, env, form)
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

    def expand_single_macro(self, name_sym, syminfo: SymbolInfo, args, env, form):
        fasl = Fasl()
        func_call_code = [S('get'), syminfo.symbol, S('ap')]
        self.assembler.assemble(func_call_code, fasl)

        machine = Secd()

        try:
            # since we want to push arguments on the stack, load the libraries
            # first to make sure they won't interfere with what we push.
            machine.load_fasls(self.lib_fasls + [self.macros_fasl])
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

    def parse_define_form(self, expr, form_name) -> tuple[Symbol, Pair]:
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
        # regular top-level define. we only set the define type as a macro to
        # env.

        name, lambda_form = self.parse_define_form(expr, 'define-macro')
        if len(expr) < 3:
            raise self._compile_error('Not enough arguments for define-macro')

        if name in env.macros:
            raise self._compile_error(
                f'Duplicate macro definition: {name}')

        info = env.lookup_symbol(name)
        self.defined_symbols[info.symbol] = DefineInfo(is_macro=True)
        if info.immutable:
            raise self._compile_error(
                f'Attempting to assign immutable variable: {name}',
                form=name)

        code = self.compile_form(lambda_form, env)
        code += [S('set'), info.symbol, S('void')]

        env.add_macro(name)

        return code, name

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
                raise self._compile_error(
                    f'define-macro only allowed at the top-level',
                    form=define_form)
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
                assert False, 'define-macro in body'
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
        if info.kind == SymbolKind.AUX:
            raise self._compile_error(
                f'Invalid use of aux keyword: {sym}',
                form=sym)
        elif info.kind == SymbolKind.PRIMCALL:
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
        elif info.kind == SymbolKind.LIBRARY:
            return [S('get'), info.symbol]
        elif info.kind == SymbolKind.FREE:
            self.read_symbols.add((info.symbol, sym, self.current_source))
            return [S('get'), info.symbol]
        elif info.kind == SymbolKind.MACRO:
            raise self._compile_error(
                f'Invalid use of macro name: {sym}',
                form=sym)
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

        # compile function before arguments, even though we need it later, so
        # that if there's an undefined error here, it's raised before any
        # undefined symbol errors for arguments. this is particularly useful if
        # the actual error is that, maybe due to a missing import, we're
        # compiling a syntax name as a function.
        #
        # this still won't work on errors other than undefined symbol, because
        # those are raised immediately, while undefined symbol errors are raised
        # at the end of compilation.
        func_code = self.compile_form(expr[0], env)

        for arg in reversed(expr.to_list()[1:]):
            secd_code += self.compile_form(arg, env)
            secd_code += [S('cons')]
        secd_code += func_code
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
            code += [S('set'), info.symbol]
            self.set_symbols.add((info.symbol, name, self.current_source))
        elif info.kind == SymbolKind.LIBRARY:
            # this should not normally be used, since library imports are
            # immutable, but maybe we'll find a use for it later.
            code += [S('set'), info.symbol]
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
        path = Path(filename)

        # return absolute names as-is
        if path.is_absolute():
            return filename

        # first search in the same directory as the only we're currently
        # compiling.
        if self.current_source and self.current_source.filename:
            cur_file_path = Path(self.current_source.filename)
            relative_file = cur_file_path.parent / filename
            if relative_file.exists():
                return str(relative_file)

        if filename.startswith('./') or filename.startswith('../'):
            # when the file is relative to . or .., don't look anywhere else.
            return None

        # then search current working directory
        if os.path.exists(filename):
            return filename

        # otherwise search any user specified search paths
        for path in self.include_paths:
            full_path = os.path.join(path, filename)
            if os.path.exists(full_path):
                return str(full_path)

        return None

    def _compile_include(self, expr, env, *, context: str, casefold: bool):
        if len(expr) < 2:
            raise self._compile_error(
                'Missing filenames in include directive')

        code = []
        for filename in expr.cdr:
            if not isinstance(filename, String):
                raise self._compile_error(
                    f'Filename in include directive not a string '
                    f'literal: {filename}')

            full_path = self.find_include_file(filename.value)
            if full_path is None:
                raise self._compile_error(
                    f'Included file not found: {filename}',
                    form=filename)

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

            old_source = self.current_source
            self.current_source = SourceFile(filename=full_path)
            if context == 'toplevel':
                include_code = self.compile_toplevel_form(begin_form, env)
            elif context == 'local':
                include_code = self.compile_form(begin_form, env)
            elif context == 'library':
                include_code = self.compile_toplevel_form(begin_form, env)
            elif context == 'library-declarations':
                include_code = []
                for expr in exprs:
                    include_code += self.compile_library_declaration(expr, env)
            else:
                assert False, 'unhandled context'

            if self.debug_info:
                include_code = [S(':filename-start'), filename] + include_code
                include_code += [S(':filename-end')]

            code += include_code
            self.current_source = old_source

        return code

    def compile_include_toplevel(self, expr, env):
        return self._compile_include(
            expr, env,
            context='toplevel',
            casefold=False)

    def compile_include_ci_toplevel(self, expr, env):
        return self._compile_include(
            expr, env,
            context='toplevel',
            casefold=True)

    def compile_include_local(self, expr, env):
        return self._compile_include(
            expr, env,
            context='local',
            casefold=False)

    def compile_include_ci_local(self, expr, env):
        return self._compile_include(
            expr, env,
            context='local',
            casefold=True)

    def _compile_cond_expand(self, expr, env, *, context: str):
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
                if context == 'toplevel':
                    code += self.compile_toplevel_form(begin_form, env)
                elif context == 'local':
                    code += self.compile_form(begin_form, env)
                elif context == 'library':
                    for expr in expressions:
                        code += self.compile_library_declaration(expr, env)
                else:
                    assert False, 'unhandled context'

        return code

    def compile_cond_expand_toplevel(self, expr, env):
        return self._compile_cond_expand(expr, env, context='toplevel')

    def compile_cond_expand_local(self, expr, env):
        return self._compile_cond_expand(expr, env, context='local')

    def compile_list(self, expr, env: Environment):
        if expr == Nil():
            raise self._compile_error(
                'Empty list is not a valid form')

        if not expr.is_proper():
            raise self._compile_error(
                f'Cannot compile improper list: {expr}')

        if isinstance(expr.car, Symbol):
            info =  env.lookup_symbol(expr.car, at_head=True)
            if info.kind == SymbolKind.AUX:
                raise self._compile_error(
                    f'Invalid use of aux keyword "{expr.car}"',
                    form=expr.car)

            special_forms = {
                SpecialForms.BEGIN: self.compile_begin,
                SpecialForms.SET: self.compile_set,
                SpecialForms.IF: self.compile_if,
                SpecialForms.LAMBDA: self.compile_lambda,
                SpecialForms.LET: self.compile_let,
                SpecialForms.LETREC: self.compile_letrec,
                SpecialForms.QUOTE: self.compile_quote,
                SpecialForms.INCLUDE: self.compile_include_local,
                SpecialForms.INCLUDE_CI: self.compile_include_ci_local,
                SpecialForms.COND_EXPAND: self.compile_cond_expand_local,
            }

            if info.kind == SymbolKind.SPECIAL:
                compile_func = special_forms.get(info.special_type)

                if not compile_func:
                    # let's try to give a more specific error first
                    if info.is_special(SpecialForms.DEFINE):
                        raise self._compile_error(
                            f'define is only allowed at the top-level or beginning of the body',
                            form=expr)
                    elif info.is_special(SpecialForms.DEFINE_MACRO):
                        raise self._compile_error(
                            f'define-macro is only allowed at the top-level or beginning of the body',
                            form=expr)
                    else:
                        # for example, define-library at non-top-level
                        # positions
                        raise self._compile_error(
                            f'Form not allowed at this position: {expr}',
                            form=expr)

                return compile_func(expr, env)
            else:
                if info.kind == SymbolKind.PRIMCALL:
                    return self.compile_primcall(expr, env, info)
                else:
                    return self.compile_func_call(expr, env)
        else:
            # if it's something that's obviously not a procedure, let's catch it
            # here at compile time.
            if not isinstance(expr.car, (Symbol, Pair)):
                raise self._compile_error(
                    f'Invalid procedure: {expr.car}',
                    form=expr.car)
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

    def compile_library_declaration(self, declaration, env: Environment):
        code = []
        if not isinstance(declaration, Pair):
            raise self._compile_error(
                f'Library declaration is not a list: {declaration}',
                form=declaration)
        if declaration.car == S('import'):
            self.process_import(declaration, env)
        elif declaration.car == S('export'):
            for export_spec in declaration.cdr:
                if isinstance(export_spec, Symbol):
                    env.add_export(export_spec, self.current_source)
                elif isinstance(export_spec, Pair):
                    if len(export_spec) != 3 or \
                       export_spec[0] != S('rename') or \
                       not isinstance(export_spec[1], Symbol) or \
                       not isinstance(export_spec[2], Symbol):
                        raise self._compile_error(
                            f'Bad export spec: {export_spec}',
                            form=export_spec)
                    env.add_renamed_export(export_spec[1], export_spec[2], self.current_source)
                else:
                    raise self._compile_error(
                        f'Bad export spec: {export_spec}',
                        form=export_spec)
        elif declaration.car == S('include'):
            code += self._compile_include(
                declaration, env, context='library', casefold=False)
        elif declaration.car == S('include-ci'):
            code += self._compile_include(
                declaration, env, context='library', casefold=True)
        elif declaration.car == S('include-library-declarations'):
            code += self._compile_include(
                declaration, env, context='library-declarations', casefold=False)
        elif declaration.car == S('cond-expand'):
            code += self._compile_cond_expand(declaration, env, context='library')
        elif declaration.car == S('begin'):
            code += self.compile_toplevel_begin(declaration, env)
        else:
            raise self._compile_error(
                f'Invalid library declaration: {declaration}',
                form=declaration)

        return code

    def compile_define_library(self, form: Pair) -> list:
        if len(form) < 2:
            raise self._compile_error(
                f'Library name not specified: {form}', form=form)

        lib_name = form[1]
        try:
            lib_name = LibraryName(lib_name.to_list())
        except ValueError:
            raise self._compile_error(
                f'Inavlid library name: {lib_name}', form=lib_name)

        code = []
        lib_env = Environment(lib_name=lib_name)
        for declaration in form.cddr():
            code += self.compile_library_declaration(declaration, lib_env)

        # check exports
        for export in lib_env.exports:
            # set at_head to true to make sure specials are also matched
            info = lib_env.lookup_symbol(export.internal, at_head=True)
            if info.kind == SymbolKind.MACRO:
                export.is_macro = True
                continue
            if info.kind != SymbolKind.FREE:
                continue
            if lib_name.mangle_symbol(export.internal) not in self.defined_symbols:
                raise self._compile_error(
                    f'No such identifier to export: {export.internal}',
                    form=export.internal,
                    source=export.export_source_file)

        # add library to available_libs so that it becomes immediately available
        # to libraries defined later
        self.defined_libs.append((lib_name, lib_env.exports))

        return code

    def compile_toplevel_begin(self, form: Pair, env: Environment):
        form_code = []
        for expr in form.cdr:
            sub_code = self.compile_toplevel_form(expr, env)
            if form_code != []:
                form_code += [S('drop')]
            form_code += sub_code
        if form_code == []:
            form_code = [S('void')]
        return form_code

    def _compile_toplevel_form(self, form, env):
        if isinstance(form, Nil):
            raise self._compile_error(
                'Empty list is not a valid form', form=form)

        if isinstance(form, Pair) and not form.is_proper():
            raise self._compile_error(
                f'Cannot compile improper list: {form}')

        form = self.macro_expand(form, env)
        if not isinstance(form, Pair):
            return self.compile_form(form, env)

        info = None
        if isinstance(form.car, Symbol):
            info = self.lookup_symbol(form.car, env, at_head=True)

        if info and info.kind == SymbolKind.AUX:
            raise self._compile_error(
                f'Invavlid use of aux keyword: {form.car}',
                form=form.car)
        elif info and info.is_special(SpecialForms.DEFINE_MACRO):
            form_code, name = self.process_define_macro(form, env)

            self.macros_fasl.add_define(name, is_macro=True)
            self.assembler.assemble(form_code, self.macros_fasl)

            # if we're not compiling a library, do not include the macro
            # code in the output.
            if not self.compiling_library:
                form_code = []
        elif info and info.is_special(SpecialForms.DEFINE):
            name_sym, value = self.parse_define_form(form, 'define')

            info = env.lookup_symbol(name_sym)
            if info.immutable:
                raise self._compile_error(
                    f'Attempting to assign immutable variable: {name_sym}',
                    form=name_sym)

            self.defined_symbols[info.symbol] = DefineInfo(is_macro=False)
            form_code = self.compile_form(value, env)

            form_code += [S('set'), info.symbol, S('void')]
            if self.debug_info:
                if form.src_start is not None and form.src_end is not None:
                    form_code = \
                        [S(':define-start'), String(name_sym.name), Integer(form.src_start)] + \
                        form_code + \
                        [S(':define-end'), Integer(form.src_end)]
            self.macros_fasl.add_define(name_sym, is_macro=False)
            self.assembler.assemble(form_code, self.macros_fasl)
        elif info and info.is_special(SpecialForms.BEGIN):
            form_code = self.compile_toplevel_begin(form, env)
        elif info and info.is_special(SpecialForms.INCLUDE):
            form_code = self.compile_include_toplevel(form, env)
        elif info and info.is_special(SpecialForms.INCLUDE_CI):
            form_code = self.compile_include_ci_toplevel(form, env)
        elif info and info.is_special(SpecialForms.COND_EXPAND):
            form_code = self.compile_cond_expand_toplevel(form, env)
        elif info and info.is_special(SpecialForms.DEFINE_LIBRARY):
            form_code = self.compile_define_library(form)
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

    def process_import_set(self, import_set: Pair) -> ImportSet:
        if import_set.car == S('only'):
            if not isinstance(import_set[1], Pair):
                raise self._compile_error(
                    f'Invalid "only" import set: {import_set[1]}',
                    form=import_set[1])
            base_set = self.process_import_set(import_set[1])
            identifiers = import_set.cddr()
            for identifier in identifiers:
                if base_set.lookup(identifier) is None:
                    raise self._compile_error(
                        f'Identifier {identifier} not exported by import set: {import_set[1]}',
                        form=identifier)
            return OnlyImportSet(base_set, identifiers)
        elif import_set.car == S('except'):
            if not isinstance(import_set[1], Pair):
                raise self._compile_error(
                    f'Invalid "except" import set: {import_set[1]}',
                    form=import_set[1])
            base_set = self.process_import_set(import_set[1])
            identifiers = import_set.cddr()
            for identifier in identifiers:
                if base_set.lookup(identifier) is None:
                    raise self._compile_error(
                        f'Identifier {identifier} not exported by import set: {import_set[1]}',
                        form=identifier)
            return ExceptImportSet(base_set, identifiers)
        elif import_set.car == S('prefix'):
            if len(import_set) != 3 or \
               not isinstance(import_set[1], Pair):
                raise self._compile_error(
                    f'Invalid "prefix" import set: {import_set[1]}',
                    form=import_set[1])
            base_set = self.process_import_set(import_set[1])
            prefix = import_set[2]
            if not isinstance(prefix, Symbol):
                raise self._compile_error(
                    f'Prefix for import set not a symbol: {prefix}',
                    form=prefix)
            return PrefixImportSet(base_set, prefix)
        elif import_set.car == S('rename'):
            if not isinstance(import_set[1], Pair):
                raise self._compile_error(
                    f'Invalid "rename" import set: {import_set[1]}',
                    form=import_set[1])
            base_set = self.process_import_set(import_set[1])
            renames = import_set.cddr()
            for rename in renames:
                if not isinstance(rename, Pair) or \
                   len(rename) != 2 or \
                   not isinstance(rename[0], Symbol) or \
                   not isinstance(rename[1], Symbol):
                    raise self._compile_error(
                        f'Invalid import rename: {rename}',
                        form=rename)
                if base_set.lookup(rename[0]) is None:
                    raise self._compile_error(
                        f'Renamed identifier "{rename[0]}" not exported by: {import_set[1]}',
                        form=rename[0])
            return RenameImportSet(base_set, renames)
        else:
            result = LibraryImportSet.get_import_set(
                LibraryName(import_set.to_list()),
                self.lib_fasls,
                local_libs=self.defined_libs)
            if result is None:
                raise self._compile_error(
                    f'Unknown library: {import_set}', form=import_set)
            return result

    def process_import(self, form: Pair, env: Environment):
        if not isinstance(form.cdr, Pair):
            raise self._compile_error(
                f'Invaid import declaration: {form}', form=form)

        sets = []
        for import_set in form.cdr:
            if not isinstance(import_set, Pair):
                raise self._compile_error(
                    f'Invaid import set: {form}', form=import_set)
            sets.append(self.process_import_set(import_set))

        for import_set in sets:
            env.add_import(import_set)

    def compile_program(self, text, env=None, *, filename=None):
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

            if isinstance(form, Pair) and form.car == S('import'):
                self.process_import(form, env)
                continue

            if len(env.import_sets) == 0:
                raise self._compile_error('No imports found')

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

        all_defines = set(self.defined_symbols.keys())
        for lib in self.lib_fasls:
            all_defines |= set(lib.defines)

        for unique_sym, sym, filename in self.set_symbols:
            if unique_sym not in all_defines:
                raise self._compile_error(
                    f'Symbol {sym} is set at some point but never defined',
                    form=sym, source=filename)

        for unique_sym, sym, filename in self.read_symbols:
            if unique_sym not in all_defines:
                for impset in env.import_sets:
                    if impset.lookup(sym) is not None:
                        break
                else:
                    raise self._compile_error(
                        f'Symbol {sym} is read at some point but never defined',
                        form=sym, source=filename)

        program = Program(
            code=code,
            defines=self.defined_symbols,
            defined_libs=self.defined_libs,
            debug_info_enabled=self.debug_info,
        )

        return program


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
        program = compiler.compile_program(text, filename=source_filename)
    except ReadError as e:
        print(f'Read error: {e}', file=sys.stderr)
        sys.exit(1)
    except CompileError as e:
        print(f'Compile error: {e}', file=sys.stderr)
        e.print_snippet()
        sys.exit(1)

    print(program.code)
