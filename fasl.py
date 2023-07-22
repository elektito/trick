import argparse
import struct

from machinetypes import List, String, Symbol, DEFAULT_ENCODING


FASL_MAGIC = b'TRCKFASL'


class FaslError(Exception):
    pass


class FaslSection:
    @property
    def name(self):
        raise NotImplementedError

    def serialize(self) -> bytes:
        raise NotImplementedError


class FaslDbgInfoSection(FaslSection):
    section_id = 1

    def __init__(self, source_file):
        self.source_file = source_file
        self.records = []

    def add_record(self, src_start, src_end, asm_start, asm_end):
        self.records.append((src_start, src_end, asm_start, asm_end))

    @property
    def name(self):
        return 'dbginfo'

    def serialize(self) -> bytes:
        result = b''

        filename = self.source_file if self.source_file else ''
        result += struct.pack('<I', len(filename))
        result += filename.encode(DEFAULT_ENCODING)

        result += struct.pack('<I', len(self.records))
        for src_start, src_end, asm_start, asm_end in self.records:
            result += struct.pack(
                '<IIII', src_start, src_end, asm_start, asm_end)

        return result

    @staticmethod
    def deserialize(data: bytes):
        section = FaslDbgInfoSection(source_file=None)

        i = 0

        filename_len, = struct.unpack('<I', data[i:i+4])
        i += 4
        filename_bytes = data[i:i+filename_len]
        i += filename_len
        filename = filename_bytes.decode(DEFAULT_ENCODING)
        if filename:
            section.source_file = filename

        nrecords, = struct.unpack('<I', data[i:i+4])
        i += 4
        for _ in range(nrecords):
            src_start, src_end, asm_start, asm_end = struct.unpack(
                '<IIII', data[i:i+16])
            section.add_record(src_start, src_end, asm_start, asm_end)
            i += 16

        assert i == len(data)

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

    def add_extra_section(self, section: FaslSection):
        self.extra_sections.append(section)

    def get_extra_section(self, name) -> (None | FaslSection):
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
            output.write(struct.pack('<I', len(s)))
            output.write(s.encode(DEFAULT_ENCODING))

        # write symbols
        for sym in self.symtab:
            strnum = self.strtab.index(String(sym.name))
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
            data = section.serialize()
            output.write(struct.pack(
                '<II', section.section_id, len(data)))
            output.write(data)

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
            section_id, size, = struct.unpack('<II', input.read(8))
            body = input.read(size)
            klass = get_class_from_section_id(section_id)
            section = klass.deserialize(body)
            fasl.extra_sections.append(section)

        return fasl


def get_class_from_section_id(section_id):
    for i in globals().values():
        if isinstance(i, type) and \
           issubclass(i, FaslSection) and \
           i != FaslSection and \
           i.section_id == section_id:
            return i
    return None


def configure_argparse(parser: argparse.ArgumentParser):
    parser.description = 'Parse fasl files.'

    parser.add_argument(
        'input', default='-', nargs='?',
        help='Input file to parse and analyze.')

    parser.set_defaults(func=main)


def main(args):
    with open(args.input, 'rb') as f:
        fasl = Fasl.load(f, args.input)

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
