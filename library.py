from enum import Enum
import re
from typing import Optional

from env import ToplevelEnvironment, Variable, VariableKind
from importsets import ImportSet
from libname import LibraryName
import runtime
from machinetypes import Integer, List, Pair, Symbol
from primcalls import primcalls
from serialization import Serializable
from symbolinfo import AuxKeywords, SpecialForms, SymbolInfo, SymbolKind
from transform import Transformer


class LibraryLookupError(Exception):
    def __init__(self, msg, form=None):
        self.msg = msg
        self.form = form

    def __repr__(self):
        return self.msg


class LibraryExportError(Exception):
    def __init__(self, msg, form=None, source=None):
        self.msg = msg
        self.form = form
        self.source = source

    def __repr__(self):
        return self.msg


class ExportKind(Serializable, Enum):
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

    def dump(self, output):
        self._dump_uint4(self.value, output)

    @classmethod
    def load(cls, input):
        return ExportKind(cls._load_uint4(input))


class LibraryExportedSymbol(Serializable):
    def __init__(self,
                 internal: Symbol,
                 external: Symbol,
                 *,
                 kind: Optional[ExportKind]=None,
                 special_type: Optional[SpecialForms]=None,
                 aux_type: Optional[AuxKeywords]=None,
                 declaration=None,
                 source_file=None):
        assert kind != ExportKind.AUX or aux_type is not None
        assert kind != ExportKind.SPECIAL or special_type is not None

        self.kind = kind
        self.internal = internal
        self.external = external

        self.special_type = special_type
        self.aux_type = aux_type

        # the following is kept so that upon finalization, if we want to raise
        # error we know where the export originally appeared.
        self.declaration = declaration
        self.source_file = source_file

    def __repr__(self):
        if self.internal == self.external:
            return f'<LibraryExportedSymbol {self.external} {self.kind}>'
        else:
            return f'<LibraryExportedSymbol {self.external} (internal={self.internal}) {self.kind}>'

    def dump(self, output):
        assert self.kind is not None

        self.internal.dump(output)
        self.external.dump(output)
        self.kind.dump(output)
        self._dump_optional(self.special_type, output)
        self._dump_optional(self.aux_type, output)

    @classmethod
    def load(cls, input):
        internal = Symbol.load(input)
        external = Symbol.load(input)
        kind = ExportKind.load(input)
        special_type = cls._load_optional(SpecialForms, input)
        aux_type = cls._load_optional(AuxKeywords, input)
        return LibraryExportedSymbol(
            internal, external,
            kind=kind,
            special_type=special_type,
            aux_type=aux_type,
        )


class Library(ToplevelEnvironment, Serializable):
    def __init__(self, name: LibraryName):
        super().__init__()
        self.name = name
        self.exports: list[LibraryExportedSymbol] = []

    def __repr__(self):
        return f'<Library {self.name}>'

    def lookup_symbol(self, sym: Symbol, *, already_mangled=False) -> SymbolInfo:
        """
        this method looks up a name from inside the library environment. core
        names and external names are looked up as is, while internal names are
        mangled first.
        """
        if sym.name.startswith('#$'):
            if info := self._core.lookup_external_name(sym):
                return info

        if sym.info is not None:
            return sym.info

        for import_set in self.import_sets:
            if info := import_set.lookup(sym):
                return info

        if already_mangled:
            assert sym.name.startswith('##')
        else:
            sym = self.name.mangle_symbol(sym)

        return super().lookup_symbol(sym)

    def lookup_external_name(self, sym: Symbol) -> (SymbolInfo | None):
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
                    var = self.lookup_symbol(e.internal, already_mangled=True)
                    assert isinstance(var.transformer, Transformer)
                    transformer = var.transformer
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

    def add_define(self, name: Symbol, kind: VariableKind, *,
                   already_mangled=False):
        if not already_mangled:
            name = self.name.mangle_symbol(name)
        super().add_define(name, kind)

    def add_macro(self, name: Symbol, transformer, *,
                  already_mangled=False):
        if not already_mangled:
            name = self.name.mangle_symbol(name)
        super().add_macro(name, transformer)

    def add_export(self, sym: Symbol, declaration, source_file):
        # notice that we don't check if the symbol actually exists here, since
        # the definition might appear later in the code. that check will be
        # performed in finalize_exports
        self.exports.append(
            LibraryExportedSymbol(
                internal=sym,
                external=sym,
                declaration=declaration,
                source_file=source_file))

    def add_renamed_export(self,
                           internal: Symbol,
                           external: Symbol,
                           declaration,
                           source_file):
        # notice that we don't check if the symbol actually exists here, since
        # the definition might appear later in the code. that check will be
        # performed in finalize_exports
        self.exports.append(
            LibraryExportedSymbol(
                internal, external,
                declaration=declaration,
                source_file=source_file))

    def finalize_exports(self):
        for export in self.exports:
            info = self.lookup_symbol(export.internal)
            if info.kind == SymbolKind.FREE:
                raise LibraryExportError(
                    f'No such identifier to export: {export.internal}',
                    form=export.internal,
                    source=export.source_file)

            export.kind = ExportKind.from_symbol_kind(info.kind)
            export.special_type = info.special_type
            export.aux_type = info.aux_type
            if info.kind in (SymbolKind.DEFINED_NORMAL,
                             SymbolKind.DEFINED_UNHYGIENIC_MACRO,
                             SymbolKind.DEFINED_MACRO):
                export.internal = info.symbol

    def get_all_names(self) -> list[Symbol]:
        return [e.external for e in self.exports]

    def dump(self, output):
        self._dump_string(self.name.mangle(), output)
        if self.name.equals('trick', 'core'):
            return

        self._dump_list(self.import_sets, output)
        self._dump_list(self.defined_symbols.values(), output)
        self._dump_list(self.exports, output)

    @classmethod
    def load(cls, input):
        name = LibraryName.unmangle(cls._load_string(input))
        if name.equals('trick', 'core'):
            return CoreLibrary()

        import_sets = cls._load_list(ImportSet, input)
        defined_symbols = cls._load_list(Variable, input)
        lib = Library(name)
        lib.import_sets = import_sets
        lib.defined_symbols = {var.name: var for var in defined_symbols}
        lib.exports = cls._load_list(LibraryExportedSymbol, input)
        return lib


class CoreLibrary(Library):
    def __init__(self):
        self.name = LibraryName.create('trick', 'core')

    def lookup_symbol(self, sym: Symbol) -> SymbolInfo:
        info = self.lookup_external_name(sym)
        if info is not None:
            return info
        return SymbolInfo(
            symbol=sym,
            kind=SymbolKind.FREE,
        )

    def lookup_external_name(self, sym: Symbol) -> (SymbolInfo | None):
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
