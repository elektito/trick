DEFAULT_ENCODING = 'ascii'


class Symbol:
    def __init__(self, name, *, unique=False):
        assert isinstance(name, str)
        self.name = name
        self.unique = unique

    def id(self):
        if self.unique:
            # unique symbols (gensyms) are only the same if they refer to the
            # same object
            return id(self)
        else:
            # normal symbols are eq if they have the same name
            return self.name

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

    def id(self):
        # booleans are eq if they have the same value
        return self.value

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


class Char:
    name_to_code = {
        'alarm': 0x07,
        'backspace': 0x08,
        'delete': 0x7f,
        'escape': 0x1b,
        'newline': 0x0a,
        'null': 0x00,
        'return': 0x0d,
        'space': 0x20,
        'tab': 0x09,
    }

    code_to_name = {
        0x07: 'alarm',
        0x08: 'backspace',
        0x7f: 'delete',
        0x1b: 'escape',
        0x0a: 'newline',
        0x00: 'null',
        0x0d: 'return',
        0x20: 'space',
        0x09: 'tab',
    }

    def __init__(self, char_code):
        self.char_code = char_code

    def __eq__(self, other):
        if not isinstance(other, Char):
            return False
        return self.char_code == other.char_code

    def __str__(self):
        name = Char.code_to_name.get(self.char_code)
        if name is None:
            char = chr(self.char_code)
            if char.isprintable:
                return f'#\\{char}'
            else:
                return f'#\\{self.char_code}'
        else:
            return f'#\\{name}'

    def __repr__(self):
        return f'<Char {self}>'

    def id(self):
        return self.char_code


class String:
    def __init__(self, value: str):
        assert isinstance(value, str)
        self.value = value

    def id(self):
        # strings are eq only if they are the same object
        return id(self)

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


class Closure:
    def __init__(self, c, e, *, nparams: int, rest_param: bool):
        self.c = c
        self.e = e
        self.nparams = nparams
        self.rest_param = rest_param

    def __repr__(self):
        if self.rest_param:
            return f'<Closure nparams={self.nparams}+rest c={self.c} e={self.e}>'
        else:
            return f'<Closure nparams={self.nparams} c={self.c} e={self.e}>'


class Continuation(Closure):
    def __init__(self, s, e, c, d):
        self.s = [i for i in s] # shallow copy
        self.e = e
        self.c = c
        self.d = [i for i in d] # shallow copy

        self.nparams = 1
        self.rest_param = False

    def __repr__(self):
        return f'<Continuation s={self.s} e={self.e} c={self.c} d={self.d}>'


class List:
    @staticmethod
    def from_list(ls):
        if ls == []:
            return Nil()
        else:
            return Pair.from_list(ls)

    @staticmethod
    def from_list_recursive(ls):
        if ls == []:
            return Nil()
        else:
            return Pair.from_list_recursive(ls)


class Nil(List):
    _instance = None

    def __new__(klass, *args, **kwargs):
        if not isinstance(klass._instance, klass):
            klass._instance = object.__new__(klass, *args, **kwargs)
        return klass._instance

    def __str__(self):
        return '()'

    def __repr__(self):
        return '<Nil>'

    def __iter__(self):
        return self

    def __next__(self):
        raise StopIteration

    def __len__(self):
        return 0

    def is_proper(self):
        return True

    def to_list(self):
        return []


class Pair(List):
    class Iterator:
        def __init__(self, start):
            self.cur = start

        def __next__(self):
            if self.cur == Nil():
                raise StopIteration

            value = self.cur.car
            self.cur = self.cur.cdr
            return value

    def __init__(self, car, cdr):
        if not isinstance(car, (int, Bool, String, Char, List, Symbol, Closure)):
            raise TypeError(f'Invalid value type for car: {car}')
        if not isinstance(cdr, (int, Bool, String, Char, List, Symbol, Closure)):
            raise TypeError(f'Invalid value type for cdr: {cdr}')

        self.car = car
        self.cdr = cdr

    def __str__(self) -> str:
        # (quote . (value . nil))
        if isinstance(self.cdr, Pair):
            if self.car == Symbol('quote'):
                return f"'{self.cdar()}"
            if self.car == Symbol('unquote'):
                return f',{self.cdar()}'
            if self.car == Symbol('unquote-splicing'):
                return f',@{self.cdar()}'
            if self.car == Symbol('backquote'):
                return f'`{self.cdar()}'

        s = f'({self.car}'

        next = self.cdr
        while isinstance(next, Pair):
            s += f' {next.car}'
            next = next.cdr

        if isinstance(next, Nil):
            s += ')'
        else:
            s += f' . {next})'

        return s

    def __repr__(self):
        return f'<Pair car={self.car} cdr={self.cdr}>'

    def __len__(self) -> int:
        if self.cdr == Nil():
            return 1
        elif isinstance(self.cdr, Pair):
            return 1 + len(self.cdr)
        else:
            raise ValueError('Cannot get the length of improper list')

    def __iter__(self):
        if not self.is_proper():
            raise ValueError('Cannot iterate over an improper list')
        return Pair.Iterator(self)

    def __getitem__(self, index):
        if not isinstance(index, int):
            raise TypeError('Only integer indices are supported')

        if index < 0:
            raise IndexError('Negative indices not supported')

        cur = self
        while True:
            if not isinstance(cur, List):
                raise ValueError('Cannot index into an improper list')
            if index == 0:
                return cur.car
            index -= 1
            if cur.cdr == Nil():
                raise IndexError
            cur = cur.cdr

    def index(self, value):
        i = 0
        cur = self
        while True:
            if cur.car == value:
                return i
            elif isinstance(cur.cdr, Pair):
                cur = self.cdr
            elif not isinstance(cur, Nil):
                raise ValueError('Index called on improper list')
            else:
                raise ValueError('Value not in list')

    def caar(self):
        return self.car.car

    def cadr(self):
        return self.car.cdr

    def cdar(self):
        return self.cdr.car

    def cddr(self):
        return self.cdr.cdr

    def before_dot(self):
        if self.cdr == Nil():
            raise ValueError('before_dot called on proper list')
        elif isinstance(self.cdr, Pair):
            return Pair(self.car, self.cdr.before_dot())
        else:
            return Pair(self.car, Nil())

    def after_dot(self):
        if self.cdr == Nil():
            raise ValueError('after_dot called on proper list')
        elif isinstance(self.cdr, Pair):
            return self.cdr.after_dot()
        else:
            return self.cdr

    def to_list(self) -> list:
        l = [self.car]
        next = self.cdr
        while isinstance(next, Pair):
            l.append(next.car)
            next = next.cdr

        if not isinstance(next, Nil):
            raise ValueError('Not a proper list')

        return l

    def to_list_recursive(self) -> list:
        if isinstance(self.car, Pair):
            l = [self.car.to_list_recursive()]
        else:
            l = [self.car]
        next = self.cdr
        while isinstance(next, Pair):
            if isinstance(next.car, Pair):
                l.append(next.car.to_list_recursive())
            else:
                l.append(next.car)
            next = next.cdr

        if not isinstance(next, Nil):
            raise ValueError('Not a proper list')

        return l

    def last(self) -> 'Pair':
        if self.cdr == Nil():
            return self
        elif not isinstance(self.cdr, Pair):
            raise ValueError('Cannot get the "last" of an improper list')
        else:
            return self.cdr.last()

    def is_proper(self) -> bool:
        if self.cdr == Nil():
            return True
        elif isinstance(self.cdr, Pair):
            return self.cdr.is_proper()
        else:
            return False

    @staticmethod
    def from_list(l: list):
        assert isinstance(l, list)
        if l == []:
            return Nil()
        else:
            start = Pair(l[0], Nil())
            prev = start
            for elem in l[1:]:
                new_pair = Pair(elem, Nil())
                prev.cdr = new_pair
                prev = new_pair

            return start

    @staticmethod
    def from_list_recursive(l: list):
        assert isinstance(l, list)
        if l == []:
            return Nil()
        else:
            start = Pair(l[0], Nil())
            prev = start
            for elem in l[1:]:
                if isinstance(elem, list):
                    elem = Pair.from_list_recursive(elem)
                new_pair = Pair(elem, Nil())
                prev.cdr = new_pair
                prev = new_pair

            return start
