class Symbol:
    symtab = []

    def __init__(self, name):
        assert isinstance(name, str)
        self.name = name

        try:
            self.number = Symbol.symtab.index(self.name)
        except ValueError:
            Symbol.symtab.append(name)
            self.number = len(Symbol.symtab) - 1

    def __eq__(self, other):
        if not isinstance(other, Symbol):
            return False
        return self.name == other.name

    def __hash__(self):
        return hash(self.name)

    def __str__(self):
        return self.name

    def __repr__(self):
        return f'<Symbol {self}>'


class Bool:
    def __init__(self, value: bool):
        if not isinstance(value, bool):
            raise TypeError('Invalid boolean value')

        self.value = value

    def is_true(self):
        return self.value

    def __bool__(self):
        return self.is_true()

    def __repr__(self):
        return f'<Bool {str(self.value).lower()}>'
