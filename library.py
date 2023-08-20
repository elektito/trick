from enum import Enum
import re
from typing import Optional

import runtime
from machinetypes import Integer, List, Pair, Symbol
from primcalls import primcalls


class LibraryLookupError(Exception):
    def __init__(self, msg, form=None):
        self.msg = msg
        self.form = form

    def __repr__(self):
        return self.msg


class SymbolKind(Enum):
    SPECIAL = 1
    AUX = 2
    PRIMCALL = 3
    LOCAL = 4
    DEFINED_NORMAL = 5
    DEFINED_UNHYGIENIC_MACRO = 6
    DEFINED_MACRO = 7
    LOCAL_MACRO = 8
    FREE = 9


class ExportKind(Enum):
    NORMAL = 1
    SPECIAL = 2
    AUX = 3
    PRIMCALL = 4
    UNHYGIENIC_MACRO = 5
    MACRO = 6

    def to_symbol_kind(self):
        return {
            ExportKind.NORMAL: SymbolKind.DEFINED_NORMAL,
            ExportKind.SPECIAL: SymbolKind.SPECIAL,
            ExportKind.AUX: SymbolKind.AUX,
            ExportKind.PRIMCALL: SymbolKind.PRIMCALL,
            ExportKind.UNHYGIENIC_MACRO: SymbolKind.DEFINED_UNHYGIENIC_MACRO,
            ExportKind.MACRO: SymbolKind.DEFINED_MACRO,
        }[self]

    @staticmethod
    def from_symbol_kind(kind: SymbolKind):
        return {
            SymbolKind.SPECIAL: ExportKind.SPECIAL,
            SymbolKind.PRIMCALL: ExportKind.PRIMCALL,
            SymbolKind.AUX: ExportKind.AUX,
            SymbolKind.DEFINED_UNHYGIENIC_MACRO: ExportKind.UNHYGIENIC_MACRO,
            SymbolKind.DEFINED_MACRO: ExportKind.MACRO,
        }.get(kind, ExportKind.NORMAL)


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
    DEFINE_SYNTAX = 'define-syntax'
    BEGIN = 'begin'
    SET = 'set!'
    IF = 'if'
    LAMBDA = 'lambda'
    LET = 'let'
    LETREC = 'letrec'
    LET_SYNTAX = 'let-syntax'
    LETREC_SYNTAX = 'letrec-syntax'
    QUOTE = 'quote'
    INCLUDE = 'include'
    INCLUDE_CI = 'include-ci'
    COND_EXPAND = 'cond-expand'
    SYNTAX_RULES = 'syntax-rules'


class SymbolInfo:
    def __init__(self, symbol: Symbol, kind: SymbolKind, *,
                 primcall_nargs=None,
                 primcall_code=None,
                 local_frame_idx=None,
                 local_var_idx=None,
                 special_type=None,
                 aux_type=None,
                 transformer=None,
                 library_name=None,
                 immutable=False):
        assert isinstance(symbol, Symbol)
        match kind:
            case SymbolKind.PRIMCALL:
                assert primcall_nargs is not None
                assert primcall_code is not None
            case SymbolKind.LOCAL:
                assert local_frame_idx is not None
                assert local_var_idx is not None
            case SymbolKind.SPECIAL:
                assert special_type is not None
            case SymbolKind.AUX:
                assert aux_type is not None
            case SymbolKind.DEFINED_MACRO:
                assert transformer is not None
            case SymbolKind.LOCAL_MACRO:
                assert transformer is not None

        self.symbol = symbol
        self.kind = kind
        self.primcall_nargs = primcall_nargs
        self.primcall_code = primcall_code
        self.local_frame_idx = local_frame_idx
        self.local_var_idx = local_var_idx
        self.special_type = special_type
        self.aux_type = aux_type
        self.transformer = transformer
        self.library_name = library_name
        self.immutable = immutable

    def is_special(self, special_type: SpecialForms) -> bool:
        return self.kind == SymbolKind.SPECIAL and \
            self.special_type == special_type

    def is_aux(self, aux_type: AuxKeywords) -> bool:
        return self.kind == SymbolKind.AUX and \
            self.aux_type == aux_type

    def is_macro(self):
        return self.kind in (SymbolKind.DEFINED_MACRO,
                             SymbolKind.LOCAL_MACRO)

    def __repr__(self):
        return f'<SymbolInfo {self.symbol} kind={self.kind.name}>'


class LibraryName:
    def __init__(self, name_parts: list[Symbol | Integer]):
        if not isinstance(name_parts, list) or \
           not all(isinstance(p, (Symbol, Integer)) for p in name_parts) or \
           any(isinstance(p, Integer) and p < 0 for p in name_parts):
            raise ValueError('Invalid library name')

        self.parts = name_parts

    def __eq__(self, other):
        if not isinstance(other, LibraryName):
            return False
        return all(i == j for i, j in zip(self.parts, other.parts))

    def __hash__(self):
        return hash(tuple(hash(p) for p in self.parts))

    def __str__(self):
        return str(List.from_list(self.parts))

    def __repr__(self):
        return f'<LibraryName {str(self)}>'

    def mangle(self) -> str:
        nparts = len(self.parts)
        mangled_parts = [self._mangle_name_part(i) for i in self.parts]
        return f'##{nparts}-' + '-'.join(mangled_parts)

    def mangle_symbol(self, sym: Symbol) -> Symbol:
        assert not sym.name.startswith('##')
        return Symbol(self.mangle() + '-' + sym.name)

    @staticmethod
    def unmangle(mangled_name: str):
        lib_name, rest = LibraryName._unmangle(mangled_name)
        if rest != '':
            raise ValueError(f'Invalid mangled name: {mangled_name}')
        return lib_name

    @staticmethod
    def unmangle_symbol(mangled_name: str):
        lib_name, rest = LibraryName._unmangle(mangled_name)
        if not rest.startswith('-'):
            raise ValueError(f'Invalid mangled name: {mangled_name}')
        return lib_name, Symbol(rest[1:])

    @staticmethod
    def _unmangle(mangled_name: str):
        m = re.match(r'##(\d+)-', mangled_name)
        if not m:
            raise ValueError(f'Invalid mangled name: {mangled_name}')

        parts = []
        nparts = int(m.group(1))
        rest = mangled_name[len(m.group(0)) - 1:]
        for _ in range(nparts):
            if not rest.startswith('-'):
                raise ValueError(f'Invalid mangled name: {mangled_name}')
            rest = rest[1:]

            if rest[0].isnumeric():
                part_size = int(rest[0])
                part = rest[1:1+part_size]
                if len(part) != part_size:
                    raise ValueError(f'Invalid mangled name: {mangled_name}')
                part = Symbol(part)
                rest = rest[1 + part_size:]
            elif rest[0] == '[':
                m = re.match(r'\[(\d+)\]', rest)
                if not m:
                    raise ValueError(f'Invalid mangled name: {mangled_name}')
                part_size = int(m.group(1))
                rest = rest[len(m.group(0)):]
                part = rest[:part_size]
                if len(part) != part_size:
                    raise ValueError(f'Invalid mangled name: {mangled_name}')
                part = Symbol(part)
                rest = rest[part_size:]
            elif rest[0] == 'i':
                m = re.match(r'i(\d+)', rest)
                if not m:
                    raise ValueError(f'Invalid mangled name: {mangled_name}')
                part = Integer(m.group(1))
                rest = rest[len(m.group(0)):]
            else:
                raise ValueError(f'Invalid mangled name: {mangled_name}')

            parts.append(part)

        return LibraryName(parts), rest

    def _mangle_name_part(self, part: (Symbol | Integer)):
        if isinstance(part, Symbol):
            if len(part.name) < 10:
                return f'{len(part.name)}{part.name}'
            else:
                return f'[{len(part.name)}]{part.name}'
        elif isinstance(part, Integer):
            return f'i{part}'
        else:
            assert False, 'unhandled name part type'


class LibraryExportedSymbol:
    def __init__(self,
                 internal: Symbol,
                 external: Symbol,
                 *,
                 kind: Optional[ExportKind]=None,
                 special_type: Optional[SpecialForms]=None,
                 aux_type: Optional[AuxKeywords]=None,
                 export_source_file=None):
        assert kind != ExportKind.AUX or aux_type is not None
        assert kind != ExportKind.SPECIAL or special_type is not None

        self.kind = kind
        self.internal = internal
        self.external = external

        self.special_type = special_type
        self.aux_type = aux_type

        self.export_source_file = export_source_file


class Library:
    def __init__(self,
                 name: LibraryName,
                 exports: list[LibraryExportedSymbol],
                 macros: dict):
        from transform import Transformer  # avoid circular reference

        assert isinstance(name, LibraryName)
        assert isinstance(exports, list)
        assert all(isinstance(i, LibraryExportedSymbol) for i in exports)
        assert isinstance(macros, dict)
        assert all(isinstance(k, Symbol) for k in macros.keys())
        assert all(isinstance(v, Transformer) for v in macros.values())

        self.name = name
        self.exports = exports
        self.macros = macros

    def __repr__(self):
        return f'<Library {self.name}>'

    def lookup(self, sym: Symbol) -> (SymbolInfo | None):
        for e in self.exports:
            if sym == e.external:
                primcall_nargs = None
                primcall_code = None
                transformer = None
                if e.kind == ExportKind.PRIMCALL:
                    prim = primcalls[e.internal.name]
                    primcall_nargs = prim['nargs']
                    primcall_code = prim['code']
                if e.kind == ExportKind.MACRO:
                    transformer = self.macros[e.internal]
                return SymbolInfo(
                    symbol=e.internal,
                    kind=e.kind.to_symbol_kind(),
                    library_name=self.name,
                    special_type=e.special_type,
                    aux_type=e.aux_type,
                    primcall_nargs=primcall_nargs,
                    primcall_code=primcall_code,
                    transformer=transformer,
                    immutable=True,
                )

        return None

    def get_all_names(self) -> list[Symbol]:
        return [e.external for e in self.exports]


class CoreLibrary(Library):
    def __init__(self):
        self.name = LibraryName([Symbol('trick'), Symbol('core')])

    def lookup(self, sym: Symbol) -> (SymbolInfo | None):
        m = re.match(r'#\$/(?P<module>\w+)/(?P<proc>\w+)', sym.name)
        if m:
            module = m.group('module')
            proc = m.group('proc')
            desc = runtime.find_proc(module, proc)
            if desc is None:
                raise LibraryLookupError(
                    f'Unknown runtime procedure: {sym.name}',
                    form=sym)

            return SymbolInfo(
                symbol=sym,
                kind=SymbolKind.PRIMCALL,
                primcall_nargs=len(desc['args']),
                primcall_code=[Symbol('trap'), Symbol(f'{module}/{proc}')],
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

        if any((sym.name == i.value or
                (sym.name.startswith('#$') and sym.name[2:] == i.value))
               for i in SpecialForms):
            if sym.name.startswith('#$'):
                special_type = SpecialForms(sym.name[2:])
            else:
                special_type = SpecialForms(sym.name)
            return SymbolInfo(
                symbol=sym,
                kind=SymbolKind.SPECIAL,
                special_type=special_type,
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

    def get_all_names(self):
        names = []
        names += [Symbol(i.value) for i in SpecialForms]
        names += [Symbol(i.value) for i in AuxKeywords]
        names += list(Symbol(i) for i in primcalls.keys())
        return names


class ImportSet:
    def lookup(self, sym: Symbol) -> (SymbolInfo | None):
        raise NotImplementedError


class LibraryImportSet(ImportSet):
    def __init__(self, lib: Library):
        self.lib = lib

    def lookup(self, sym: Symbol) -> (SymbolInfo | None):
        try:
            return self.lib.lookup(sym)
        except LibraryLookupError as e:
            raise CompileError(str(e), form=e.form)

    def get_all_names(self):
        return self.lib.get_all_names()

    def __str__(self):
        return f'<LibraryImportSet {self.lib.name}>'

    def __repr__(self):
        return str(self)


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
            elif sym == from_name:
                # the original name is not available anymore
                return None
        return self.base_import_set.lookup(sym)

    def __str__(self):
        renames = [f'"{f}"=>"{t}"' for f, t in self.renames]
        renames = ' '.join(renames)
        return f'<RenameImportSet base={self.base_import_set} renames=({renames})>'

    def __repr__(self):
        return str(self)
