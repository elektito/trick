import re

from .machinetypes import Integer, List, Symbol


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

    def equals(self, *parts):
        if len(parts) != len(self.parts):
            return False

        for given_part, self_part in zip(parts, self.parts):
            if isinstance(given_part, str):
                given_part = Symbol(given_part)
            if isinstance(given_part, int):
                given_part = Integer(given_part)
            if given_part != self_part:
                return False

        return True

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
            if len(part.name) < 10 and \
               (len(part.name) == 0 or not part.name[0].isnumeric()):
                return f'{len(part.name)}{part.name}'
            else:
                return f'[{len(part.name)}]{part.name}'
        elif isinstance(part, Integer):
            return f'i{part}'
        else:
            assert False, 'unhandled name part type'

    @staticmethod
    def create(*parts):
        fixed_parts = []
        for p in parts:
            if isinstance(p, str):
                p = Symbol(p)
            elif isinstance(p, int):
                p = Integer(p)
            fixed_parts.append(p)

        return LibraryName(fixed_parts)
