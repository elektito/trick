DEFAULT_ENCODING = 'ascii'


class Symbol:
    def __init__(self, name, *, unique=False):
        assert isinstance(name, str)
        self.name = name
        self.unique = unique

    def __eq__(self, other):
        if not isinstance(other, Symbol):
            return False
        elif self.unique:
            return id(self) == id(other)
        else:
            return self.name == other.name

    def __hash__(self):
        return hash(self.name)

    def __str__(self):
        return self.name

    def __repr__(self):
        if self.unique:
            return f'<Symbol {self.name} (unique)>'
        else:
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

    def __str__(self):
        return '#t' if self.value else '#f'

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
