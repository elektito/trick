from enum import Enum
import re
import struct
from typing import Optional
from machinetypes import Integer, List, Symbol


class SymbolKind(Enum):
    SPECIAL = 1
    AUX = 2
    PRIMCALL = 3
    LOCAL = 4
    DEFINED_NORMAL = 5
    DEFINED_MACRO = 6
    FREE = 7
    TRANSFORMER = 8


class ExportKind(Enum):
    NORMAL = 1
    SPECIAL = 2
    AUX = 3
    PRIMCALL = 4
    MACRO = 5

    def to_symbol_kind(self):
        return {
            ExportKind.NORMAL: SymbolKind.DEFINED_NORMAL,
            ExportKind.SPECIAL: SymbolKind.SPECIAL,
            ExportKind.AUX: SymbolKind.AUX,
            ExportKind.PRIMCALL: SymbolKind.PRIMCALL,
            ExportKind.MACRO: SymbolKind.DEFINED_MACRO,
        }[self]

    @staticmethod
    def from_symbol_kind(kind: SymbolKind):
        return {
            SymbolKind.SPECIAL: ExportKind.SPECIAL,
            SymbolKind.PRIMCALL: ExportKind.PRIMCALL,
            SymbolKind.AUX: ExportKind.AUX,
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
        self.kind = kind
        self.internal = internal
        self.external = external

        self.special_type = special_type
        self.aux_type = aux_type

        self.export_source_file = export_source_file
