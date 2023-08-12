import argparse
import io
import struct
from collections import defaultdict

from library import LibraryName
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

    def __init__(self,
                 lib_names: list[LibraryName],
                 exports: list[tuple[Symbol, Symbol]]):
        self.lib_names = lib_names
        self.exports = exports

    @property
    def name(self):
        return 'libinfo'

    def __repr__(self):
        return f'<FaslLibInfoSection>'

    def _serialize(self) -> bytes:
        s = b''
        s += struct.pack('<I', len(self.lib_names))
        for name in self.lib_names:
            s += serialize_string(name.mangle())

        s += struct.pack('<I', len(self.exports))
        for internal, external in self.exports:
            s += serialize_string(internal.name)
            s += serialize_string(external.name)

        return s

    @staticmethod
    def _deserialize(s):
        names = []
        n, = struct.unpack('<I', s[:4])
        offset = 4
        for _ in range(n):
            mangled_name, offset = deserialize_string(s, offset)
            lib_name = LibraryName.unmangle(mangled_name)
            names.append(lib_name)

        exports = []
        n, = struct.unpack('<I', s[offset:offset+4])
        offset += 4
        for _ in range(n):
            internal, offset = deserialize_string(s, offset)
            external, offset = deserialize_string(s, offset)
            exports.append((internal, external))

        return FaslLibInfoSection(names, exports)


class DefineInfo:
    def __init__(self, is_macro=False):
        self.is_macro = is_macro

    def dump(self, output):
        value = 1 if self.is_macro else 0
        output.write(struct.pack('<I', value))

    @staticmethod
    def load(input):
        value = input.read(4)
        value, = struct.unpack('<I', value)
        is_macro = (value == 1)
        info = DefineInfo(is_macro=is_macro)
        return info

    def __repr__(self):
        return f'<DefineInfo is_macro={self.is_macro}>'


class Fasl:
    def __init__(self, *, filename=None):
        self.filename = filename
        self.strtab = []
        self.symtab = []
        self.defines = {}
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

    def add_define(self, sym: Symbol, is_macro=False):
        assert isinstance(sym, Symbol)
        if sym not in self.symtab:
            self.add_symbol(sym)
        self.defines[sym] = DefineInfo(
            is_macro=is_macro)

    def add_symbol(self, sym: Symbol) -> int:
        assert isinstance(sym, Symbol)
        if String(sym.name) not in self.strtab:
            self.add_string(String(sym.name))

        try:
            return self.symtab.index(sym)
        except ValueError:
            self.symtab.append(sym)
            return len(self.symtab) - 1

    def add_string(self, s: String) -> int:
        assert isinstance(s, String)

        try:
            return self.strtab.index(s)
        except ValueError:
            self.strtab.append(s)
            return len(self.strtab) - 1

    def dump(self, output):
        # write fasl header
        output.write(FASL_MAGIC)
        output.write(struct.pack(
            '<BIIIII',
            FASL_VERSION,              # version
            len(self.strtab),          # number of string ltierals
            len(self.symtab),          # number of symbols
            len(self.defines),         # number of globally defined symbols
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

        # write defines
        for sym, info in self.defines.items():
            symnum = self.symtab.index(sym)
            output.write(struct.pack('<I', symnum))
            info.dump(output)

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

        headers = input.read(21)
        version, nstrs, nsyms, ndefines, csize, nsecs = struct.unpack(
            '<BIIIII', headers)

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

        for i in range(ndefines):
            symnum, = struct.unpack('<I', input.read(4))
            sym = fasl.symtab[symnum]
            info = DefineInfo.load(input)
            fasl.defines[sym] = info

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
        print(f'[{i}] {r.symbol_name} src={r.src_start}-{r.src_end} asm={r.asm_start}-{r.asm_end}')
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

    print(f'{len(fasl.defines)} definitions:')
    for name, info in fasl.defines.items():
        print(f'   {name}{" (M)" if info.is_macro else ""}')

    lib_info = fasl.get_section('libinfo')
    if lib_info:
        print()
        print(f'{len(lib_info.lib_names)} libraries available: ', end='')
        print(', '.join(str(n) for n in lib_info.lib_names))

        lib_to_exports = defaultdict(list)
        for mangled_internal, external in lib_info.exports:
            lib_name, internal = LibraryName.unmangle_symbol(mangled_internal)
            lib_to_exports[lib_name].append((internal, external))

        for lib_name, exports in lib_to_exports.items():
            exports.sort(key=lambda r: r[1])
            print(f'   {lib_name} has {len(exports)} export(s):')
            for internal, external in exports:
                print(f'      exported={external}  internal={internal}')

        for lib_name in lib_info.lib_names:
            if lib_name not in lib_to_exports:
                print(f'   {lib_name} has 0 exports.')


def main(args):
    with open(args.input, 'rb') as f:
        fasl = Fasl.load(f, args.input)

    if args.dbg_records:
        print_dbg_records(fasl)
    else:
        print_general_info(fasl)
