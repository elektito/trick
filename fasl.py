import argparse
import io
import struct
from collections import defaultdict

from library import AuxKeywords, ExportKind, Library, LibraryExportedSymbol, LibraryName, SpecialForms
from machinetypes import Integer, List, String, Symbol, DEFAULT_ENCODING
from snippet import show_snippet


FASL_MAGIC = b'TRCKFASL'
FASL_VERSION = 1


class FaslError(Exception):
    pass


class FaslSection:
    section_id = None

    @property
    def name(self):
        raise NotImplementedError

    def dump(self, file):
        data = self._serialize()
        file.write(struct.pack('<II', self.section_id, len(data)))
        file.write(data)

    def _serialize(self) -> bytes:
        raise NotImplementedError

    @staticmethod
    def load(file):
        section_id, size = struct.unpack('<II', file.read(8))
        data = file.read(size)

        klass = FaslSection.find_fasl_section_class_by_id(section_id)
        if klass is None:
            print(f'Unknown FASL section id: {section_id}')
            return None
        else:
            return klass._deserialize(data)

    @staticmethod
    def _deserialize(data: bytes) -> 'FaslSection':
        raise NotImplementedError

    @staticmethod
    def find_fasl_section_class_by_id(section_id):
        for i in globals().values():
            if isinstance(i, type) and \
               issubclass(i, FaslSection) and \
               i != FaslSection and \
               i.section_id == section_id:
                return i
        return None


class DbgInfoRecord:
    record_id = None

    def dump(self, file):
        data = self._serialize()
        file.write(struct.pack('<II', self.record_id, len(data)))
        file.write(data)

    def _serialize(self) -> bytes:
        raise NotImplementedError

    @staticmethod
    def load(file):
        record_id, size = struct.unpack('<II', file.read(8))
        data = file.read(size)

        klass = DbgInfoRecord.find_dbginfo_record_class_by_id(record_id)
        if klass is None:
            print(f'Unknown debug info record id: {record_id}')
            return None
        else:
            return klass._deserialize(data)

    @staticmethod
    def _deserialize(data: bytes):
        raise NotImplementedError

    @staticmethod
    def find_dbginfo_record_class_by_id(record_id):
        for i in globals().values():
            if isinstance(i, type) and \
               issubclass(i, DbgInfoRecord) and \
               i != DbgInfoRecord and \
               i.record_id == record_id:
                return i
        return None


class DbgInfoExprRecord(DbgInfoRecord):
    record_id = 1

    def __init__(self, src_start, src_end, asm_start, asm_end):
        self.src_start = src_start
        self.src_end = src_end
        self.asm_start = asm_start
        self.asm_end = asm_end

    def __repr__(self):
        return \
            f'<DbgInfoExprRecord src={self.src_start}-{self.src_end} ' \
            f'asm={self.asm_start}-{self.asm_end}>'

    def _serialize(self) -> bytes:
        return struct.pack(
            '<IIII',
            self.src_start,
            self.src_end,
            self.asm_start,
            self.asm_end)

    @staticmethod
    def _deserialize(data: bytes) -> 'DbgInfoExprRecord':
        src_start, src_end, asm_start, asm_end = \
            struct.unpack('<IIII', data)
        record = DbgInfoExprRecord(
            src_start, src_end, asm_start, asm_end)
        return record


class DbgInfoDefineRecord(DbgInfoRecord):
    record_id = 2

    def __init__(self,
                 symbol_name: str,
                 src_start: int,
                 src_end: int,
                 asm_start: int,
                 asm_end: int):
        self.symbol_name = symbol_name
        self.src_start = src_start
        self.src_end = src_end
        self.asm_start = asm_start
        self.asm_end = asm_end

    def __repr__(self):
        return \
            f'<DbgInfoDefineRecord {self.symbol_name} ' \
            f'src={self.src_start}-{self.src_end} ' \
            f'asm={self.asm_start}-{self.asm_end}>'

    def _serialize(self) -> bytes:
        result = serialize_string(self.symbol_name)
        result += struct.pack(
            '<IIII',
            self.src_start,
            self.src_end,
            self.asm_start,
            self.asm_end)
        return result

    @staticmethod
    def _deserialize(data: bytes) -> 'DbgInfoDefineRecord':
        offset = 0
        symbol_name, offset = deserialize_string(data, offset)
        src_start, src_end, asm_start, asm_end = \
            struct.unpack('<IIII', data[offset:offset+16])
        record = DbgInfoDefineRecord(
            symbol_name, src_start, src_end, asm_start, asm_end)
        return record


class DbgInfoSourceFileRecord(DbgInfoRecord):
    record_id = 3

    def __init__(self,
                 filename: str,
                 asm_start: int,
                 asm_end: int):
        self.filename = filename
        self.asm_start = asm_start
        self.asm_end = asm_end

    def __repr__(self):
        return \
            f'<DbgInfoFilenameRecord {self.filename} ' \
            f'asm={self.asm_start}-{self.asm_end}>'

    def _serialize(self) -> bytes:
        result = serialize_string(self.filename)
        result += struct.pack(
            '<II',
            self.asm_start,
            self.asm_end)
        return result

    @staticmethod
    def _deserialize(data: bytes) -> 'DbgInfoSourceFileRecord':
        offset = 0
        filename, offset = deserialize_string(data, offset)
        asm_start, asm_end = \
            struct.unpack('<II', data[offset:offset+16])
        record = DbgInfoSourceFileRecord(
            filename, asm_start, asm_end)
        return record


class FaslDbgInfoSection(FaslSection):
    section_id = 1

    def __init__(self):
        self.records = []

    def __repr__(self):
        return f'<FaslDbgInfoSection nrecords={len(self.records)}>'

    def add_record(self, record):
        self.records.append(record)

    @property
    def name(self):
        return 'dbginfo'

    def _serialize(self) -> bytes:
        result = b''
        result += struct.pack('<I', len(self.records))
        for r in self.records:
            file = io.BytesIO()
            r.dump(file)
            result += file.getvalue()
        return result

    @staticmethod
    def _deserialize(data: bytes):
        section = FaslDbgInfoSection()

        offset = 0
        nrecords, = struct.unpack('<I', data[offset:offset+4])
        offset += 4
        file = io.BytesIO(data[offset:])
        for _ in range(nrecords):
            record = DbgInfoRecord.load(file)
            if record:
                section.add_record(record)

        return section

    def get_source_file(self, asm_offset):
        """Given an asm offset, return the source filename, if a matching source
        file record exists"""
        for r in self.records:
            if not isinstance(r, DbgInfoSourceFileRecord):
                continue
            if r.asm_start <= asm_offset < r.asm_end:
                return r.filename
        return None


class FaslLibInfoSection(FaslSection):
    section_id = 2

    def __init__(self):
        self.libs: list[Library] = []

    @property
    def name(self):
        return 'libinfo'

    def add_library(self, lib: Library):
        self.libs.append(lib)

    def __repr__(self):
        return f'<FaslLibInfoSection>'

    def _serialize(self) -> bytes:
        s = b''
        s += struct.pack('<I', len(self.libs))
        for lib in self.libs:
            s += serialize_string(lib.name.mangle())
            s += struct.pack('<I', len(lib.exports))
            for export in lib.exports:
                s += serialize_string(export.internal.name)
                s += serialize_string(export.external.name)

                s += struct.pack('<I', export.kind.value)

                if export.special_type is None:
                    special_idx = -1
                else:
                    special_idx = list(SpecialForms).index(export.special_type)
                s += struct.pack('<i', special_idx)

                if export.aux_type is None:
                    aux_idx = -1
                else:
                    aux_idx = list(AuxKeywords).index(export.aux_type)
                s += struct.pack('<i', aux_idx)

        return s

    @staticmethod
    def _deserialize(s):
        nlibs, = struct.unpack('<I', s[:4])
        offset = 4
        section = FaslLibInfoSection()
        for _ in range(nlibs):
            mangled_name, offset = deserialize_string(s, offset)
            lib_name = LibraryName.unmangle(mangled_name)

            exports = []
            nexports, = struct.unpack('<I', s[offset:offset+4])
            offset += 4
            for _ in range(nexports):
                internal, offset = deserialize_string(s, offset)
                external, offset = deserialize_string(s, offset)
                internal = Symbol(internal)
                external = Symbol(external)

                kind_int, = struct.unpack('<I', s[offset:offset+4])
                kind = ExportKind(kind_int)
                offset += 4

                special_idx, = struct.unpack('<i', s[offset:offset+4])
                offset += 4
                if special_idx < 0:
                    special_type = None
                else:
                    special_type = list(SpecialForms)[special_idx]

                aux_idx, = struct.unpack('<i', s[offset:offset+4])
                offset += 4
                if special_idx < 0:
                    aux_type = None
                else:
                    aux_type = list(AuxKeywords)[aux_idx]

                exports.append(
                    LibraryExportedSymbol(
                        internal, external,
                        kind=kind,
                        special_type=special_type,
                        aux_type=aux_type))
            # FIXME last argument to Library is empty because we don't support
            # loading macros from a FASL yet.
            section.add_library(Library(lib_name, exports, {}))

        return section


class Fasl:
    def __init__(self, *, filename=None):
        self.filename = filename
        self.strtab = []
        self.symtab = []
        self.code = b''
        self.sections = []

    def __repr__(self):
        if self.filename:
            return f'<Fasl {self.filename}>'
        else:
            return '<Fasl>'

    def add_section(self, section: FaslSection):
        self.sections.append(section)

    def get_section(self, name):
        for section in self.sections:
            if section.name == name:
                return section

        return None

    def add_symbol(self, sym: Symbol) -> int:
        assert isinstance(sym, Symbol)
        if String(sym.name) not in self.strtab:
            self.add_string(String(sym.name))

        try:
            return self.symtab.index(sym)
        except ValueError:
            self.symtab.append(sym)
            return len(self.symtab) - 1

    def add_string(self, new_string: String) -> int:
        assert isinstance(new_string, String)

        # String objects don't (and shouldn't) have __eq__, so we can't use
        # self.strtab.index(...) here
        for i, s in enumerate(self.strtab):
            if s.value == new_string.value:
                return i

        self.strtab.append(new_string)
        return len(self.strtab) - 1

    def dump(self, output):
        # write fasl header
        output.write(FASL_MAGIC)
        output.write(struct.pack(
            '<BIIII',
            FASL_VERSION,              # version
            len(self.strtab),          # number of string ltierals
            len(self.symtab),          # number of symbols
            len(self.code),            # code sizes
            len(self.sections),  # number of sections
        ))

        # write string literals
        for s in self.strtab:
            output.write(serialize_string(s))

        # write symbols
        for sym in self.symtab:
            # can't do `self.strtab.index(String(sym.name))` because String
            # objects do not implement content-based __eq__ (and we don't want
            # them to).
            strnum = [s.value for s in self.strtab].index(sym.name)
            output.write(struct.pack('<I', strnum))

        # write code
        output.write(self.code)

        # write sections
        for section in self.sections:
            section.dump(output)

    @staticmethod
    def load(input, filename=None):
        fasl = Fasl(filename=filename)

        magic = input.read(8)
        if magic != FASL_MAGIC:
            raise FaslError('Bad magic')

        headers = input.read(17)
        version, nstrs, nsyms, csize, nsecs = struct.unpack(
            '<BIIII', headers)

        if version != FASL_VERSION:
            raise FaslError(f'Unsupported version: {version}')

        for i in range(nstrs):
            size, = struct.unpack('<I', input.read(4))
            string = input.read(size).decode(DEFAULT_ENCODING)
            fasl.strtab.append(String(string))

        for i in range(nsyms):
            strnum, = struct.unpack('<I', input.read(4))
            sym = Symbol(fasl.strtab[strnum].value)
            fasl.symtab.append(sym)

        fasl.code = input.read(csize)

        for i in range(nsecs):
            section = FaslSection.load(input)
            if section:
                fasl.sections.append(section)

        return fasl


def serialize_string(string: str) -> bytes:
    result = struct.pack('<I', len(string))
    result += string.encode(DEFAULT_ENCODING)
    return result


def deserialize_string(data: bytes, offset: int) -> tuple[str, int]:
    length, = struct.unpack('<I', data[offset:offset+4])
    offset += 4
    string_bytes = data[offset:offset+length]
    offset += length
    string = string_bytes.decode(DEFAULT_ENCODING)
    return string, offset


def configure_argparse(parser: argparse.ArgumentParser):
    parser.description = 'Parse fasl files.'

    parser.add_argument(
        'input', default='-', nargs='?',
        help='Input file to parse and analyze.')

    parser.add_argument(
        '--dbg-records', '-G', action='store_true',
        help='Display all debug records with their associated source '
        'code.')

    parser.set_defaults(func=main)


def print_dbg_records(fasl: Fasl):
    sources = {}
    def get_source(asm_offset):
        """Return source text for the given asm offset, if an appropriate source
        file record exists."""
        filename = dbginfo.get_source_file(asm_offset)
        if filename is None:
            return ''

        if filename in sources:
            return sources[filename]

        try:
            with open(filename) as f:
                text = f.read()
        except FileNotFoundError:
            return ''

        sources[filename] = text

        return text

    dbginfo = fasl.get_section('dbginfo')
    if dbginfo is None:
        print('No debug info.')
        return

    defines = []
    exprs = []
    source_files = []
    unknown = []
    for r in dbginfo.records:
        if isinstance(r, DbgInfoExprRecord):
            exprs.append(r)
        elif isinstance(r, DbgInfoDefineRecord):
            defines.append(r)
        elif isinstance(r, DbgInfoSourceFileRecord):
            source_files.append(r)
        else:
            unknown.append(r)

    if not exprs:
        print('No expression records')
    else:
        print('Expression records:')
    for i, r in enumerate(exprs, 1):
        print(f'[{i}] src={r.src_start}-{r.src_end} asm={r.asm_start}-{r.asm_end}')
        text = get_source(r.asm_start)
        if text:
            show_snippet(text, r.src_start, r.src_end)

    print()
    if not defines:
        print('No define records')
    else:
        print('Define records:')
    for i, r in enumerate(defines, 1):
        symbol_name = r.symbol_name
        if symbol_name.startswith('##'):
            lib_name, unmangled_name = LibraryName.unmangle_symbol(symbol_name)
            symbol_name = f'{unmangled_name} in library {lib_name}'
        print(f'[{i}] {symbol_name} src={r.src_start}-{r.src_end} asm={r.asm_start}-{r.asm_end}')
        text = get_source(r.asm_start)
        if text:
            print(text[r.src_start:r.src_end])

    print()
    if not source_files:
        print('No source file records')
    else:
        print('Source file records:')
    for i, r in enumerate(source_files, 1):
        print(f'[{i}] {r.filename} asm={r.asm_start}-{r.asm_end}')

    if len(unknown) > 0:
        print()
        print(f'There are {len(unknown)} unknown record(s).')


def print_general_info(fasl: Fasl):
    strings = List.from_list_recursive(fasl.strtab)
    symbols = List.from_list_recursive(fasl.symtab)

    print(f'Code size: {len(fasl.code)} byte(s)')
    print()

    print(f'{len(strings)} string literal(s):')
    for s in strings:
        print(f'   {s}')
    print()

    print(f'{len(symbols)} symbol(s):')
    for s in symbols:
        print(f'   {s}')
    print()

    lib_info = fasl.get_section('libinfo')
    if lib_info:
        nlibs = len(lib_info.libs)
        print()
        print(f'{nlibs} librar{"ies" if nlibs != 1 else "y"} available: ', end='')
        print(', '.join(str(lib.name) for lib in lib_info.libs()))

        for lib in lib_info.libs:
            exports = lib.exports.sort(key=lambda r: r.external.name)
            print(f'   {lib.name} has {len(exports)} export(s):')
            for ex in exports:
                kind = ''
                if ex.kind != ExportKind.NORMAL:
                    kind = f' [{ex.kind.name}]'
                if ex.internal == ex.external:
                    print(f'      {ex.external}' + kind)
                else:
                    print(f'      {ex.external} (internal: {ex.internal})' + kind)


def main(args):
    with open(args.input, 'rb') as f:
        fasl = Fasl.load(f, args.input)

    if args.dbg_records:
        print_dbg_records(fasl)
    else:
        print_general_info(fasl)
