import argparse
import io
import struct

from machinetypes import List, String, Symbol, DEFAULT_ENCODING
from snippet import show_snippet


FASL_MAGIC = b'TRCKFASL'


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


class FaslDbgInfoSection(FaslSection):
    section_id = 1

    def __init__(self, source_file):
        self.source_file = source_file
        self.records = []

    def __repr__(self):
        return f'<FaslDbgInfoSection nrecords={len(self.records)}>'

    def add_record(self, record):
        self.records.append(record)

    @property
    def name(self):
        return 'dbginfo'

    def _serialize(self) -> bytes:
        filename = self.source_file if self.source_file else ''
        result = b''
        result += serialize_string(filename)
        result += struct.pack('<I', len(self.records))
        for r in self.records:
            file = io.BytesIO()
            r.dump(file)
            result += file.getvalue()
        return result

    @staticmethod
    def _deserialize(data: bytes):
        section = FaslDbgInfoSection(source_file=None)

        offset = 0
        filename, offset = deserialize_string(data, offset)
        if filename:
            section.source_file = filename

        nrecords, = struct.unpack('<I', data[offset:offset+4])
        offset += 4
        file = io.BytesIO(data[offset:])
        for _ in range(nrecords):
            record = DbgInfoRecord.load(file)
            if record:
                section.add_record(record)

        return section


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
        self.extra_sections = []

    def __repr__(self):
        if self.filename:
            return f'<Fasl {self.filename}>'
        else:
            return '<Fasl>'

    def add_extra_section(self, section: FaslSection):
        self.extra_sections.append(section)

    def get_extra_section(self, name):
        for section in self.extra_sections:
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
        output.write(b'TRCKFASL')  # magic
        output.write(struct.pack(
            '<IIIII',
            len(self.strtab),          # number of string ltierals
            len(self.symtab),          # number of symbols
            len(self.defines),         # number of globally defined symbols
            len(self.code),            # code sizes
            len(self.extra_sections),  # number of sections
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
        for section in self.extra_sections:
            section.dump(output)

    @staticmethod
    def load(input, filename=None):
        fasl = Fasl(filename=filename)

        magic = input.read(8)
        if magic != FASL_MAGIC:
            raise FaslError('Bad magic')

        headers = input.read(20)
        nstrs, nsyms, ndefines, csize, nsecs = struct.unpack(
            '<IIIII', headers)

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
                fasl.extra_sections.append(section)

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


def print_dbg_records(fasl):
    dbginfo = fasl.get_extra_section('dbginfo')
    if dbginfo is None:
        print('No debug info.')
        return

    if dbginfo.source_file:
        with open(dbginfo.source_file) as f:
            text = f.read()
    else:
        print('No source file indicated in debug info.')
        text = ''

    defines = []
    exprs = []
    unknown = []
    for r in dbginfo.records:
        if isinstance(r, DbgInfoExprRecord):
            exprs.append(r)
        elif isinstance(r, DbgInfoDefineRecord):
            defines.append(r)
        else:
            unknown.append(r)

    if not exprs:
        print('No expression records')
    else:
        print('Expression records:')
    for i, r in enumerate(exprs, 1):
        print(f'[{i}] src={r.src_start}-{r.src_end} asm={r.asm_start}-{r.asm_end}')
        if text:
            #print(text[r.src_start:r.src_end])
            show_snippet(text, r.src_start, r.src_end)

    print()
    if not defines:
        print('No define records')
    else:
        print('Define records:')
    for i, r in enumerate(defines, 1):
        print(f'[{i}] {r.symbol_name} src={r.src_start}-{r.src_end} asm={r.asm_start}-{r.asm_end}')
        if text:
            print(text[r.src_start:r.src_end])
            #show_snippet(text, r.src_start, r.src_end)

    if len(unknown) > 0:
        print()
        print(f'There are {len(unknown)} unknown record(s).')


def print_general_info(fasl):
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


def main(args):
    with open(args.input, 'rb') as f:
        fasl = Fasl.load(f, args.input)

    if args.dbg_records:
        print_dbg_records(fasl)
    else:
        print_general_info(fasl)
