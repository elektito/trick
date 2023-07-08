from ctypes import ArgumentError


DEFAULT_ENCODING = 'ascii'


class Symbol:
    def __init__(self, name, interned_form=None, *, magic=None):
        if magic != 'mellon':
            raise Exception('Symbols should be created using a symtab')

        assert isinstance(name, str)
        self.name = name
        self.interned_form = interned_form

    def __eq__(self, other):
        if not isinstance(other, Symbol):
            return False
        elif self.interned_form is not None:
            return self.interned_form == other.interned_form
        else:
            return id(self) == id(other)

    def __hash__(self):
        if self.interned_form:
            return hash(self.interned_form)
        else:
            return hash(self.name)

    def __str__(self):
        return self.name

    def __repr__(self):
        return f'<Symbol {self.name}>'


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

    def __eq__(self, other):
        if not isinstance(other, Bool):
            return False
        return self.value == other.value


class String:
    def __init__(self, value: str):
        assert isinstance(value, str)
        self.value = value

    def __hash__(self):
        return hash(self.value)

    def __eq__(self, other):
        if not isinstance(other, String):
            return False
        return self.value == other.value

    def __len__(self):
        return len(self.value)

    def __repr__(self):
        return f'"{self.value}"'

    def encode(self, encoding=DEFAULT_ENCODING):
        return self.value.encode(encoding)

    @staticmethod
    def from_bytes(b: bytes) -> 'String':
        return String(b.decode(DEFAULT_ENCODING))
