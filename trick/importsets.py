from .exceptions import CompileError
from .libloader import LibLoader
from .libname import LibraryName
from .machinetypes import Pair, Symbol
from .serialization import Serializable
from .symbolinfo import SymbolInfo


class ImportSetParseError(CompileError):
    pass


class ImportSet(Serializable):
    def lookup(self, sym: Symbol) -> (SymbolInfo | None):
        raise NotImplementedError

    def lookup_internal(self, sym: Symbol) -> (SymbolInfo | None):
        raise NotImplementedError

    @property
    def underlying_lib(self) -> LibraryName:
        raise NotImplementedError

    @staticmethod
    def _get_serializable_subclasses():
        return [
            LibraryImportSet,
            OnlyImportSet,
            ExceptImportSet,
            PrefixImportSet,
            RenameImportSet,
        ]

    @staticmethod
    def parse(desc) -> 'ImportSet':
        if not isinstance(desc, Pair):
            raise ImportSetParseError(
                f'Invalid import set: {desc}',
                form=desc)

        if desc.car == Symbol('only'):
            if not isinstance(desc[1], Pair):
                raise ImportSetParseError(
                    f'Invalid "only" import set: {desc[1]}',
                    form=desc[1])
            base_set = ImportSet.parse(desc[1])
            identifiers = desc.cdr.cdr
            for identifier in identifiers:
                if base_set.lookup(identifier) is None:
                    raise ImportSetParseError(
                        f'Identifier {identifier} not exported by import set: {desc[1]}',
                        form=identifier)
            return OnlyImportSet(base_set, identifiers)
        elif desc.car == Symbol('except'):
            if not isinstance(desc[1], Pair):
                raise ImportSetParseError(
                    f'Invalid "except" import set: {desc[1]}',
                    form=desc[1])
            base_set = ImportSet.parse(desc[1])
            identifiers = desc.cdr.cdr
            for identifier in identifiers:
                if base_set.lookup(identifier) is None:
                    raise ImportSetParseError(
                        f'Identifier {identifier} not exported by import set: {desc[1]}',
                        form=identifier)
            return ExceptImportSet(base_set, identifiers)
        elif desc.car == Symbol('prefix'):
            if len(desc) != 3 or \
               not isinstance(desc[1], Pair):
                raise ImportSetParseError(
                    f'Invalid "prefix" import set: {desc[1]}',
                    form=desc[1])
            base_set = ImportSet.parse(desc[1])
            prefix = desc[2]
            if not isinstance(prefix, Symbol):
                raise ImportSetParseError(
                    f'Prefix for import set not a symbol: {prefix}',
                    form=prefix)
            return PrefixImportSet(base_set, prefix)
        elif desc.car == Symbol('rename'):
            if not isinstance(desc[1], Pair):
                raise ImportSetParseError(
                    f'Invalid "rename" import set: {desc[1]}',
                    form=desc[1])
            base_set = ImportSet.parse(desc[1])
            renames = desc.cdr.cdr
            for rename in renames:
                if not isinstance(rename, Pair) or \
                   len(rename) != 2 or \
                   not isinstance(rename[0], Symbol) or \
                   not isinstance(rename[1], Symbol):
                    raise ImportSetParseError(
                        f'Invalid import rename: {rename}',
                        form=rename)
                if base_set.lookup(rename[0]) is None:
                    raise ImportSetParseError(
                        f'Renamed identifier "{rename[0]}" not exported by: {desc[1]}',
                        form=rename[0])
            return RenameImportSet(base_set, renames)
        else:
            lib_name = desc.to_list()
            lib_name = LibraryName(lib_name)
            result = LibraryImportSet(lib_name, lazy=False)
            return result


class LibraryImportSet(ImportSet):
    serialization_id = 1

    def __init__(self, lib_name: LibraryName, *, lazy=True):
        assert isinstance(lib_name, LibraryName)

        self.lib_name = lib_name
        self._loaded_lib = None

        if not lazy:
            self._lib()

    def lookup(self, sym: Symbol) -> (SymbolInfo | None):
        return self._lib().lookup_external_name(sym)

    def lookup_internal(self, sym: Symbol) -> (SymbolInfo | None):
        return self._lib().lookup_internal_name(sym)

    def get_all_names(self):
        return self._lib().get_all_names()

    def __repr__(self):
        return f'<LibraryImportSet {self.lib_name}>'

    def _lib(self):
        if self._loaded_lib is None:
            self._loaded_lib = LibLoader().load_lib(self.lib_name)

        return self._loaded_lib

    @property
    def underlying_lib(self) -> LibraryName:
        return self.lib_name

    def _dump(self, output):
        self._dump_string(self.lib_name.mangle(), output)

    @classmethod
    def _load(cls, input):
        lib_name = cls._load_string(input)
        lib_name = LibraryName.unmangle(lib_name)
        return LibraryImportSet(lib_name)


class OnlyImportSet(ImportSet):
    serialization_id = 2

    def __init__(self, base_import_set: ImportSet, identifiers: list[Symbol]):
        self.base_import_set = base_import_set
        self.identifiers = identifiers

    def lookup(self, sym: Symbol):
        if sym not in self.identifiers:
            return None

        return self.base_import_set.lookup(sym)

    def lookup_internal(self, sym: Symbol) -> (SymbolInfo | None):
        return self.base_import_set.lookup_internal(sym)

    @property
    def underlying_lib(self) -> LibraryName:
        return self.base_import_set.underlying_lib

    def __str__(self):
        return f'<OnlyImportSet base={self.base_import_set} only={self.identifiers}>'

    def __repr__(self):
        return str(self)

    def _dump(self, output):
        self.base_import_set.dump(output)
        self._dump_list(self.identifiers, output)

    @classmethod
    def _load(cls, input):
        base_import_set = ImportSet.load(input)
        identifiers = cls._load_list(Symbol, input)
        return OnlyImportSet(base_import_set, identifiers)


class ExceptImportSet(ImportSet):
    serialization_id = 3

    def __init__(self, base_import_set: ImportSet, identifiers: list[Symbol]):
        self.base_import_set = base_import_set
        self.identifiers = identifiers

    def lookup(self, sym: Symbol):
        if sym in self.identifiers:
            return None

        return self.base_import_set.lookup(sym)

    def lookup_internal(self, sym: Symbol) -> (SymbolInfo | None):
        return self.base_import_set.lookup_internal(sym)

    @property
    def underlying_lib(self) -> LibraryName:
        return self.base_import_set.underlying_lib

    def __str__(self):
        return f'<ExceptImportSet base={self.base_import_set} except={self.identifiers}>'

    def __repr__(self):
        return str(self)

    def _dump(self, output):
        self.base_import_set.dump(output)
        self._dump_list(self.identifiers, output)

    @classmethod
    def _load(cls, input):
        base_import_set = ImportSet.load(input)
        identifiers = cls._load_list(Symbol, input)
        return ExceptImportSet(base_import_set, identifiers)


class PrefixImportSet(ImportSet):
    serialization_id = 4

    def __init__(self, base_import_set: ImportSet, prefix: Symbol):
        self.base_import_set = base_import_set
        self.prefix = prefix.name

    def lookup(self, sym: Symbol):
        if not sym.name.startswith(self.prefix):
            return None

        no_prefix_name = Symbol(sym.name[len(self.prefix):])
        result = self.base_import_set.lookup(no_prefix_name)
        if result is None:
            return None
        return result

    def lookup_internal(self, sym: Symbol) -> (SymbolInfo | None):
        return self.base_import_set.lookup_internal(sym)

    @property
    def underlying_lib(self) -> LibraryName:
        return self.base_import_set.underlying_lib

    def __str__(self):
        return f'<PrefixImportSet base={self.base_import_set} prefix="{self.prefix}">'

    def __repr__(self):
        return str(self)

    def _dump(self, output):
        self.base_import_set.dump(output)
        self._dump_string(self.prefix, output)

    @classmethod
    def _load(cls, input):
        base_import_set = ImportSet.load(input)
        prefix = cls._load_string(input)
        return PrefixImportSet(base_import_set, Symbol(prefix))


class RenameImportSet(ImportSet):
    serialization_id = 5

    def __init__(self, base_import_set: ImportSet, renames: list[Pair]):
        self.base_import_set = base_import_set
        self.renames = renames

    def lookup(self, sym: Symbol):
        for rename in self.renames:
            from_name = rename[0]
            to_name = rename[1]
            if sym == to_name:
                result =  self.base_import_set.lookup(from_name)
                return result
            elif sym == from_name:
                # the original name is not available anymore
                return None
        return self.base_import_set.lookup(sym)

    def lookup_internal(self, sym: Symbol) -> (SymbolInfo | None):
        return self.base_import_set.lookup_internal(sym)

    @property
    def underlying_lib(self) -> LibraryName:
        return self.base_import_set.underlying_lib

    def __str__(self):
        renames = [f'"{f}"=>"{t}"' for f, t in self.renames]
        renames = ' '.join(renames)
        return f'<RenameImportSet base={self.base_import_set} renames=({renames})>'

    def __repr__(self):
        return str(self)

    def _dump(self, output):
        self.base_import_set.dump(output)
        from_names = [i.car for i in self.renames]
        to_names = [i.cdr for i in self.renames]
        self._dump_list(from_names, output)
        self._dump_list(to_names, output)

    @classmethod
    def _load(cls, input):
        base_import_set = ImportSet.load(input)
        from_names = cls._load_list(Symbol, input)
        to_names = cls._load_list(Symbol, input)
        renames = [Pair(i, j) for i, j in zip(from_names, to_names)]
        return RenameImportSet(base_import_set, renames)
