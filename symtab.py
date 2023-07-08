from machinetypes import String, Symbol

MAGIC = 'mellon'

class Symtab:
    def __init__(self):
        self.interned_names = []
        self.gensym_counter = 0

    def intern(self, name: str):
        assert isinstance(name, str)

        try:
            symnum = self.interned_names.index(name)
        except ValueError:
            self.interned_names.append(name)
            symnum = len(self.interned_names) - 1

        return Symbol(name, symnum, magic=MAGIC)

    def find_by_number(self, symnum: int):
        if 0 <= symnum < len(self.interned_names):
            name = self.interned_names[symnum]
            return Symbol(name, symnum, magic=MAGIC)
        else:
            return None

    def gensym(self):
        self.gensym_counter += 1
        name = f'#:{self.gensym_counter}'
        return Symbol(name, magic=MAGIC)

    def dump(self, strtab):
        return [
            strtab.index(String(name))
            for name in self.interned_names
        ]

    def load(self, strnums, strtab):
        self.interned_names = []
        for strnum in strnums:
            self.interned_names.append(strtab[strnum].value)
