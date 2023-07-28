from uuid import uuid4


DEFAULT_ENCODING = 'utf-8'


class Reference:
    """
    Used internally to represent references in shared structures like #0# in
    #0=(1 2 #0#)
    """

    def __init__(self, label):
        self.label = label

    def __hash__(self):
        return hash(self.label)

    def __eq__(self, other):
        if not isinstance(other, Reference):
            return False
        return self.label == other.label

    def __repr__(self):
        return f'<Ref #{self.label}#>'


class Integer(int):
    def __new__(cls, n, *args, **kwargs):
        obj = super().__new__(cls, n, *args, **kwargs)
        obj.src_start = None
        obj.src_end = None
        return obj

    def __add__(self, other):
        return Integer(int(self) + other)

    def __sub__(self, other):
        return Integer(int(self) - other)

    def __mul__(self, other):
        return Integer(int(self) * other)

    def __floordiv__(self, other):
        return Integer(int(self) // other)

    def __mod__(self, other):
        return Integer(int(self) % other)

    def __abs__(self):
        return Integer(abs(int(self)))

    def __lshift__(self, other):
        return Integer(int(self) << other)

    def __rshift__(self, other):
        return Integer(int(self) >> other)

    def __and__(self, other):
        return Integer(int(self) & other)

    def __or__(self, other):
        return Integer(int(self) | other)

    def __xor__(self, other):
        return Integer(int(self) ^ other)

    def __invert__(self):
        return Integer(~int(self))

    def __neg__(self):
        return Integer(-int(self))

    def __pos__(self):
        return self


class Symbol:
    gensym_number = 0

    def __init__(self, name, *, short_name=None):
        assert isinstance(name, str)
        assert not short_name or isinstance(short_name, str)

        self.name = name

        # if short_name is not set and the name has the same format as a gensym
        # name, parse the short name. so for #:uuuuniiiquuueeid-12, we set the
        # short name to #:12.
        if short_name is None and name.startswith('#:') and '-' in name:
            after_dash = name[name.index('-')+1:]
            short_name = f'#:{after_dash}'

        if short_name:
            self.short_name = short_name
        else:
            self.short_name = name

        self.src_start = None
        self.src_end = None

    def __eq__(self, other):
        if not isinstance(other, Symbol):
            return False
        else:
            return self.name == other.name

    def __hash__(self):
        return hash(self.name)

    def __str__(self):
        return self.short_name

    def __repr__(self):
        if self.short_name != self.name:
            return f'<Symbol {self.short_name} (full={self.name})>'
        else:
            return f'<Symbol {self.name}>'

    @staticmethod
    def gensym():
        Symbol.gensym_number += 1
        gensym_number = Symbol.gensym_number
        uid = str(uuid4()).replace('-', '')

        short_name = f'#:{gensym_number}'
        full_name = f'#:{uid}-{gensym_number}'
        return Symbol(full_name, short_name=short_name)


class Bool:
    def __init__(self, value: bool):
        if not isinstance(value, bool):
            raise TypeError('Invalid boolean value')

        self.value = value

        self.src_start = None
        self.src_end = None

    def is_true(self):
        return self.value

    def __bool__(self):
        return self.is_true()

    def __str__(self):
        return '#t' if self.value else '#f'

    def __repr__(self):
        return f'<Bool {str(self.value).lower()}>'

    def __hash__(self):
        return hash(self.value)

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
        assert isinstance(char_code, int)
        self.char_code = char_code

        self.src_start = None
        self.src_end = None

    def __eq__(self, other):
        if not isinstance(other, Char):
            return False
        return self.char_code == other.char_code

    def __hash__(self):
        return hash(self.char_code)

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


class String:
    def __init__(self, value: str):
        assert isinstance(value, str)
        self.value = value

        self.src_start = None
        self.src_end = None

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


class Procedure:
    def __init__(self, c, e, fasl, *, nparams: int, rest_param: bool):
        self.fasl = fasl
        self.c = c
        self.e = e
        self.nparams = nparams
        self.rest_param = rest_param

        self.src_start = None
        self.src_end = None

    def __repr__(self):
        return '#<procedure>'


class Continuation(Procedure):
    def __init__(self, s, e, c, d, fasl, *, kind=None):
        self.fasl = fasl
        self.s = s.copy()
        self.e = e
        self.c = c
        self.d = [i for i in d] # shallow copy

        self.nparams = 0
        self.rest_param = True

        self.src_start = None
        self.src_end = None

        self.kind = kind

    def __repr__(self):
        return f'#<continuation>'


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
            klass._instance.src_start = None
            klass._instance.src_end = None
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
        if not isinstance(car, all_types):
            raise TypeError(f'Invalid value type for car: {car}')
        if not isinstance(cdr, all_types):
            raise TypeError(f'Invalid value type for cdr: {cdr}')

        self.car = car
        self.cdr = cdr

        self.src_start = None
        self.src_end = None

    def __str__(self) -> str:
        from print import Printer # avoid circular import
        printer = Printer(self)
        return printer.print()

    def __repr__(self):
        return str(self)

    def __len__(self) -> int:
        length = 0
        visited = set()
        cur = self
        while not isinstance(cur, Nil):
            length += 1

            if cur in visited:
                if isinstance(cur.cdr, Nil):
                    return length
                else:
                    print(cur, cur.is_proper())
                    print(cur.cdr, cur.cdr.is_proper())
                    raise ValueError(
                    'Cannot calculate the length of an improper list')

            visited.add(cur)

            if not isinstance(cur.cdr, (Nil, Pair)):
                raise ValueError(
                    'Cannot calculate the length of an improper list')
            cur = cur.cdr

        return length

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
        visited = set()
        cur = self
        while True:
            if isinstance(cur.cdr, Nil):
                return True
            if not isinstance(cur.cdr, Pair):
                return False
            if cur.cdr in visited:
                return False
            visited.add(cur)
            cur = cur.cdr


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
            if isinstance(l[0], list):
                start = Pair(Pair.from_list_recursive(l[0]), Nil())
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


class Values:
    def __init__(self, values):
        assert isinstance(values, list)
        self._values = values

        self.src_start = None
        self.src_end = None

    def as_list(self):
        return self._values

    def __getitem__(self, index):
        return self._values[index]

    def __len__(self):
        return len(self._values)

    def __eq__(self, other):
        raise ValueError('Attempting to compare a Values object')

    def __repr__(self):
        return f'<Values {self._values}>'


class Vector:
    class Iterator:
        def __init__(self, vector):
            self._vector = vector
            self._i = 0

        def __next__(self):
            if self._i >= len(self._vector):
                raise StopIteration

            value = self._vector[self._i]
            self._i += 1
            return value

    def __init__(self, elements):
        self._elements = [i for i in elements]

    def __iter__(self):
        return Vector.Iterator(self)

    def __len__(self):
        return len(self._elements)

    def __getitem__(self, idx: int):
        return self._elements[idx]

    def __setitem__(self, idx: int, value):
        self._elements[idx] = value

    def __str__(self):
        from print import Printer # avoid circular import
        printer = Printer(self)
        return printer.print()

    def __repr__(self):
        return str(self)

    def to_trick_list(self):
        return List.from_list(self._elements)

    def to_python_list(self):
        return [i for i in self._elements]

    @staticmethod
    def from_list_recursive(ls: list):
        return Vector([
            Vector.from_list_recursive(e) if isinstance(e, list) else e
            for e in ls
        ])


all_types = (
    Reference,
    Integer,
    Bool,
    String,
    Char,
    List,
    Symbol,
    Procedure,
    Vector
)
shareable_types = (Pair, Vector)
