import math
from fractions import Fraction
from uuid import uuid4
from serialization import Serializable

from utils import STR_ENCODING


class TrickType(Serializable):
    @staticmethod
    def _get_serializable_subclasses():
        return [
            Void,
            Integer,
            Rational,
            Float,
            Symbol,
            Bool,
            Char,
            String,
            Nil,
            Pair,
            Vector,
        ]


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


class Void(TrickType):
    serialization_id = 1
    _instance = None

    def __new__(klass, *args, **kwargs):
        if not isinstance(klass._instance, klass):
            klass._instance = object.__new__(klass, *args, **kwargs)
            klass._instance.src_start = None
            klass._instance.src_end = None
        return klass._instance

    def __str__(self):
        return '#<void>'

    def __repr__(self):
        return '#<void>'

    def _dump(self, output):
        # nothing to serialize!
        pass

    @staticmethod
    def _load(input):
        return Void()



class Number(TrickType):
    @staticmethod
    def from_python_number(n):
        if isinstance(n, int):
            n = Integer(n)
        elif isinstance(n, Fraction):
            if n.denominator == 1:
                n = Integer(n)
            else:
                n = Rational(n)
        elif isinstance(n, float):
            n = Float(n)
        elif isinstance(n, complex):
            real = Number.from_python_number(n.real)
            imag = Number.from_python_number(n.imag)
            n = Complex(real, imag)
        else:
            assert False, 'unhandled case'

        return n

    def to_python_number(self):
        raise NotImplementedError

    def to_complex(self):
        if isinstance(self, Complex):
            return self
        else:
            return Complex(self, Integer(0))

    def to_float(self):
        if isinstance(self, Complex):
            raise ValueError

        if isinstance(self, Rational):
            return Float(self.frac)
        else:
            return Float(self)

    def is_zero(self):
        if isinstance(self, Complex):
            return self.real == 0 and self.imag == 0
        else:
            return self.to_python_number() == 0

    def is_negative_zero(self):
        if isinstance(self, Complex):
            return self.real.is_negative_zero() and self.imag.is_exact_zero()
        else:
            # copy the sign of the number to 1 and then compare it. Comparing
            # against -0.0 itself won't work, since in terms of python
            # operators, -0.0==0.0 (and -0.0<0.0 is False).
            return math.copysign(1, self.to_python_number()) < 0

    def is_exact_zero(self):
        if isinstance(self, Complex):
            return self.exact and self.imag.is_zero() and self.real.is_zero()
        else:
            return isinstance(self, Integer) and self.to_python_number() == 0

    def to_specific(self):
        if isinstance(self, Rational):
            if self.frac.denominator == 1:
                return Integer(self.frac.numerator)
            else:
                return self
        elif isinstance(self, Complex):
            if self.imag.is_exact_zero():
                return self.real.to_specific()
            else:
                return self
        else:
            return self

    def __eq__(self, other):
        if not isinstance(other, Number):
            return False

        if self.exact != other.exact:
            return False

        if isinstance(self, Complex) or isinstance(other, Complex):
            n1 = self.to_complex()
            n2 = other.to_complex()
            return n1.real == n2.real and n1.imag == n2.imag

        if self.is_negative_zero() != other.is_negative_zero():
            return False

        return self.to_python_number() == other.to_python_number()

    def __hash__(self):
        return hash(self.to_python_number())

    def __abs__(self):
        raise NotImplementedError

    def __add__(self, other):
        if isinstance(self, Complex) or isinstance(other, Complex):
            n1 = self.to_complex()
            n2 = other.to_complex()
            real = n1.real + n2.real
            imag = n1.imag + n2.imag
            result = Complex(real, imag)
            result = result.to_specific()
        else:
            n1 = self.to_python_number()
            n2 = other
            if isinstance(other, TrickType):
                n2 = other.to_python_number()

            result = n1 + n2
            result = Number.from_python_number(result)
        return result

    def __sub__(self, other):
        if isinstance(self, Complex) or isinstance(other, Complex):
            n1 = self.to_complex()
            n2 = other.to_complex()
            real = n1.real - n2.real
            imag = n1.imag - n2.imag
            result = Complex(real, imag)
            result = result.to_specific()
        else:
            n1 = self.to_python_number()
            n2 = other
            if isinstance(other, TrickType):
                n2 = other.to_python_number()

            result = n1 - n2
            result = Number.from_python_number(result)
        return result

    def __mul__(self, other):
        if self.is_exact_zero() or (isinstance(other, Number) and other.is_exact_zero()):
            # multiplying an exact zero with any number (even an infinite),
            # results in an exact zero. notice that this is not true for inexact
            # 0.0 (which results in nan if multiplied with infinity)
            return Integer(0)
        elif isinstance(other, (int, float)) and other == 0:
            return Integer(0)

        if isinstance(self, Complex) or isinstance(other, Complex):
            n1 = self.to_complex()
            n2 = other.to_complex()
            real = n1.real * n2.real - n1.imag * n2.imag
            imag = n1.imag * n2.real + n1.real * n2.imag
            result = Complex(real, imag)
            result = result.to_specific()
        else:
            n1 = self.to_python_number()
            n2 = other
            if isinstance(other, TrickType):
                n2 = other.to_python_number()

            result = n1 * n2
            result = Number.from_python_number(result)
        return result

    def __truediv__(self, other):
        if self.is_nan() or (isinstance(other, Number) and other.is_nan()):
            if isinstance(self, Complex) or isinstance(other, Complex):
                return Complex(Float('nan'), Float('nan'))
            else:
                return Float('nan')

        if isinstance(self, Complex) or isinstance(other, Complex):
            n1 = self.to_complex()
            n2 = other.to_complex()
            real = n1.real * n2.real + n1.imag * other.imag
            imag = n1.imag * n2.real - n1.real * n2.imag
            div_size = n2.real * n2.real + n2.imag * n2.imag
            if div_size.is_exact_zero():
                raise ZeroDivisionError
            if div_size.is_zero():
                real = Float('inf') * n1.real.get_sign() * n2.real.get_sign()
                imag = Float('inf') * n1.imag.get_sign() * n2.real.get_sign()
                return Complex(real, imag)
            result = Complex(real / div_size, imag / div_size)
            result = result.to_specific()
        else:
            if isinstance(other, Integer) and other.is_exact_zero():
                raise ZeroDivisionError
            elif (isinstance(other, Float) and other.is_zero()) or \
               other == 0.0:
                if self.is_zero():
                    return Float('nan')
                if self.is_infinite():
                    return self
                return Float('inf') * Float(self.get_sign()) * Float(other.get_sign())
            elif other == 0.0:
                if self.is_zero():
                    return Float('nan')
                if self.is_infinite():
                    return self
                other_sign = Float(math.copysign(1, other))
                return Float('inf') * Float(self.get_sign()) * other_sign

            n1 = self.to_python_number()
            n2 = other
            if isinstance(other, TrickType):
                n2 = other.to_python_number()

            if isinstance(n1, int) and isinstance(n2, int):
                result = Fraction(n1, n2)
            else:
                result = n1 / n2
            result = Number.from_python_number(result)
        return result

    def __ge__(self, other):
        if isinstance(self, Complex) or isinstance(other, Complex):
            raise ValueError('No order defined on complex numbers')
        n1 = self.to_python_number()
        n2 = other
        if isinstance(other, Number):
            n2 = other.to_python_number()

        result = n1 >= n2
        return result

    def __le__(self, other):
        if isinstance(self, Complex) or isinstance(other, Complex):
            raise ValueError('No order defined on complex numbers')
        n1 = self.to_python_number()
        n2 = other
        if isinstance(other, Number):
            n2 = other.to_python_number()

        result = n1 <= n2
        return result

    def __gt__(self, other):
        if isinstance(self, Complex) or isinstance(other, Complex):
            raise ValueError('No order defined on complex numbers')
        n1 = self.to_python_number()
        n2 = other
        if isinstance(other, Number):
            n2 = other.to_python_number()

        result = n1 > n2
        return result

    def __lt__(self, other):
        if isinstance(self, Complex) or isinstance(other, Complex):
            raise ValueError('No order defined on complex numbers')
        n1 = self.to_python_number()
        n2 = other
        if isinstance(other, Number):
            n2 = other.to_python_number()

        result = n1 < n2
        return result


class Rational(Number):
    serialization_id = 2
    exact = True

    def __init__(self, frac: Fraction):
        assert isinstance(frac, Fraction)
        self.frac = frac

    def is_infinite(self):
        return False

    def is_nan(self):
        return False

    def __repr__(self):
        return str(self.frac)

    def __hash__(self):
        return hash(self.frac)

    def __abs__(self):
        return Rational(abs(self.frac))

    def __neg__(self):
        return Rational(-self.frac)

    def to_python_number(self):
        return self.frac


class Integer(Number, int):
    serialization_id = 3
    exact = True

    def __new__(cls, n, *args, **kwargs):
        obj = super().__new__(cls, n, *args, **kwargs)
        obj.src_start = None
        obj.src_end = None
        return obj

    def is_infinite(self):
        return False

    def is_nan(self):
        return False

    def get_sign(self) -> int:
        return 0 if self == 0 else 1 if self > 0 else -1

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

    def to_python_number(self):
        return int(self)

    def _dump(self, output):
        self._dump_int8(self, output)

    @classmethod
    def _load(cls, input):
        return Integer(cls._load_int8(input))


class Float(Number, float):
    serialization_id = 4
    exact = False

    def __new__(cls, n, *args, **kwargs):
        obj = super().__new__(cls, n, *args, **kwargs)
        obj.src_start = None
        obj.src_end = None
        return obj

    def is_infinite(self):
        return self == float('inf') or self == float('-inf')

    def is_nan(self):
        return math.isnan(self)

    def get_sign(self) -> int:
        return math.copysign(1, self)

    def __repr__(self):
        if math.isnan(self):
            return '+nan.0'
        elif float(self) == float('inf'):
            return '+inf.0'
        elif float(self) == float('-inf'):
            return '-inf.0'
        else:
            return str(float(self))

    def __mod__(self, other):
        return Float(float(self) % other)

    def __abs__(self):
        return Float(abs(float(self)))

    def __neg__(self):
        return Float(-float(self))

    def __pos__(self):
        return self

    def to_python_number(self):
        return float(self)

    def _dump(self, output):
        self._dump_float64(self, output)

    @classmethod
    def _load(cls, input):
        return Float(cls._load_float64(input))


class Complex(Number):
    serialization_id = 5

    def __init__(self, real, imag):
        assert isinstance(real, (Float, Rational, Integer))
        assert isinstance(real, (Float, Rational, Integer))
        self.real = real.to_specific()
        self.imag = imag.to_specific()

    def is_infinite(self):
        return self.real == float('inf') or \
            self.real == float('-inf') or \
            self.imag == float('inf') or \
            self.imag == float('-inf')

    def is_nan(self):
        return self.real.is_nan() or self.imag.is_nan()

    def __repr__(self):
        op = ''
        imag = str(self.imag)
        if not imag.startswith('+') and not imag.startswith('-'):
            op = '+' if self.imag >= 0 else ''
        return f'{self.real}{op}{self.imag}i'

    def __hash__(self):
        return hash((self.real, self.imag))

    def __eq__(self, other):
        if not isinstance(other, Number):
            return False
        other = other.to_complex()
        return self.real == other.real and self.imag == other.imag

    @property
    def exact(self):
        return self.real.exact and self.imag.exact

    def _dump(self, output):
        for part in [self.real, self.imag]:
            if isinstance(part, Float):
                type_id = 1
            elif isinstance(part, Rational):
                type_id = 2
            elif isinstance(part, Integer):
                type_id = 3
            else:
                assert False, 'unhandled case'

            self._dump_byte(type_id, output)
            self.real.dump(output)

    @classmethod
    def _load(cls, input):
        parts = []
        for _ in range(2):
            type_id = cls._load_byte(input)
            if type_id == 1:
                part = Float.load(input)
            elif type_id == 2:
                part = Rational.load(input)
            elif type_id == 3:
                part = Integer.load(input)
            else:
                assert False, 'unhandled case'

            parts.append(part)

        return Complex(*parts)


class Symbol(TrickType):
    serialization_id = 5
    gensym_number = 0

    def __init__(self, name, *, short_name=None, info=None, original=None, transform_env=None):
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

        # information added by macros

        # a SymbolInfo object representing the meaning of this symbol at the
        # transformer site
        self.info = info

        # the original symbol which this is a renaming of
        self.original = original

        # the transform environment; used by the compiler to resolve the local
        # variables that are referenced relative to the transformer environment.
        self.transform_env = transform_env

    def __eq__(self, other):
        if not isinstance(other, Symbol):
            return False
        else:
            return self.name == other.name

    def __hash__(self):
        return hash(self.name)

    def __str__(self):
        if self.name == '':
            return '||'

        try:
            n = int(self.name)
            return f'|{n}|'
        except ValueError:
            pass

        s = ''
        need_delimeters = False
        separators = '[]()\'"` '
        escapes = {
            '\a': '\\a',
            '\b': '\\b',
            '\t': '\\t',
            '\n': '\\n',
            '\r': '\\r',
            '\\': '\\\\',
            '|': '\\|',
        }
        for c in self.short_name:
            if c in separators:
                need_delimeters = True

            if c in escapes:
                need_delimeters = True
                s += escapes[c]
            elif not c.isprintable():
                s += f'\\x{hex(ord(c))};'
            else:
                s += c

        if need_delimeters:
            s = f'|{s}|'

        return s

    def __repr__(self):
        if self.short_name != self.name:
            return f'<Symbol {self} (full={self.name})>'
        else:
            return f'<Symbol {self}>'

    @staticmethod
    def gensym(short_name=None, *, no_shortname_prefix=False):
        uid = str(uuid4()).replace('-', '')

        if isinstance(short_name, String):
            short_name = short_name.value
        elif isinstance(short_name, Symbol):
            if short_name.short_name:
                short_name = short_name.short_name
                if short_name.startswith('#:'):
                    short_name = short_name[2:]
            else:
                short_name = short_name.name
        else:
            short_name = None

        if short_name is not None:
            full_name = f'#:{uid}-{short_name}'

            if not no_shortname_prefix:
                short_name = f'#:{short_name}'
        else:
            Symbol.gensym_number += 1
            gensym_number = Symbol.gensym_number
            short_name = f'#:{gensym_number}'
            full_name = f'#:{uid}-{gensym_number}'

        return Symbol(full_name, short_name=short_name)

    def _dump(self, output):
        self._dump_string(self.name, output)

    @classmethod
    def _load(cls, input):
        return Symbol(cls._load_string(input))


class Bool(TrickType):
    serialization_id = 6

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

    def _dump(self, output):
        self._dump_byte(1 if self.value else 0, output)

    @classmethod
    def _load(cls, input):
        value = cls._load_byte(input)
        return Bool(value != 0)


class Char(TrickType):
    serialization_id = 7

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
        return int(self.char_code) == int(other.char_code)

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

    def _dump(self, output):
        self._dump_uint4(self.char_code, output)

    @classmethod
    def _load(cls, input):
        return Char(cls._load_uint4(input))


class String(TrickType):
    serialization_id = 8

    def __init__(self, value: str):
        assert isinstance(value, str)
        self.value = value

        self.src_start = None
        self.src_end = None

    def __hash__(self):
        return hash(self.value)

    def __len__(self):
        return len(self.value)

    def __str__(self):
        s = ''
        map = {
            '\a': '\\a',
            '\b': '\\b',
            '\t': '\\t',
            '\n': '\\n',
            '\r': '\\r',
            '"': '\\"',
            '\\': '\\\\',
        }
        for c in self.value:
            escaped = map.get(c)
            if escaped is not None:
                s += escaped
            elif not c.isprintable():
                s += f'\\x{hex(ord(c))};'
            else:
                s += c
        return f'"{s}"'

    def __repr__(self):
        return f'<String {self}>'

    def encode(self, encoding=STR_ENCODING):
        return self.value.encode(encoding)

    @staticmethod
    def from_bytes(b: bytes) -> 'String':
        return String(b.decode(STR_ENCODING))

    def _dump(self, output):
        self._dump_string(self.value, output)

    @classmethod
    def _load(cls, input):
        return String(cls._load_string(input))


class Procedure(TrickType):
    def __init__(self, c, e, fasl, *, nparams: int, rest_param: bool):
        self.fasl = fasl
        self.c = c
        self.e = e
        self.nparams = nparams
        self.rest_param = rest_param

        self.src_start = None
        self.src_end = None

    def accepts_argument_count(self, n: int):
        if self.rest_param:
            return n == self.nparams
        else:
            return n >= self.nparams

    def __repr__(self):
        if self.rest_param:
            return f'#<procedure args={self.nparams}+rest>'
        else:
            return f'#<procedure args={self.nparams}>'


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


class List(TrickType):
    def __init__(self):
        self.src_start = None
        self.src_end = None

    @staticmethod
    def from_list(ls, *, after_dot=None):
        if ls == []:
            return Nil()
        else:
            return Pair.from_list(ls, after_dot=after_dot)

    @staticmethod
    def from_list_recursive(ls):
        if ls == []:
            return Nil()
        else:
            return Pair.from_list_recursive(ls)

    def to_list(self) -> list:
        raise NotImplementedError

    def to_list_recursive(self):
        raise NotImplementedError

    def split_improper_tail(self):
        raise NotImplementedError


class Nil(List):
    serialization_id = 9
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

    def __bool__(self):
        # we need this because otherwise python considers Nil() to be a true
        # value, likely because its __len__ is zero. that means we wouldn't be
        # able to use a nil value in an expression like `form or other_form` and
        # expect to get `other_form` when form is not None.
        return True

    def is_proper(self):
        return True

    def to_list(self):
        return []

    def to_list_recursive(self):
        return []

    def split_improper_tail(self):
        return self, None

    def _dump(self, output):
        # nothing to serialize!
        pass

    @classmethod
    def _load(cls, input):
        return Nil()


class Pair(List):
    serialization_id = 10

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
        if not isinstance(car, (TrickType, Reference)):
            raise TypeError(f'Invalid value type for car: {car}')
        if not isinstance(cdr, (TrickType, Reference)):
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

    def split_improper_tail(self):
        cur = self
        proper = []
        while isinstance(cur, Pair):
            proper.append(cur.car)
            cur = cur.cdr
        if isinstance(cur, Nil):
            return proper, None
        else:
            return proper, cur


    @staticmethod
    def from_list(l: list, *, after_dot=None):
        assert isinstance(l, list)
        if l == []:
            return Nil()
        else:
            start = Pair(l[0], Nil())
            last = start
            for elem in l[1:]:
                new_pair = Pair(elem, Nil())
                last.cdr = new_pair
                last = new_pair

            if after_dot is not None:
                last.cdr = after_dot

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

    def _dump(self, output):
        # FIXME this does not work with shared/cyclic lists
        #       low-prio since we're not using it for now!
        proper, tail = self.split_improper_tail()
        self._dump_list(proper, output)
        self._dump_optional(tail, output)

    @classmethod
    def _load(cls, input):
        # FIXME this does not work with shared/cyclic lists
        #       low-prio since we're not using it for now!
        proper = cls._load_list(TrickType, input)
        tail = cls._load_optional(TrickType, input)
        ls = Pair.from_list(proper)
        if tail is not None:
            cur = ls
            while isinstance(cur, Pair):
                prev = cur
                cur = cur.cdr
            prev.cdr = tail
        return ls



class Values(TrickType):
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


class Vector(TrickType):
    serialization_id = 11

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

    def _dump(self, output):
        self._dump_list(self._elements, output)

    @classmethod
    def _load(cls, input):
        return Vector(cls._load_list(TrickType, input))


class Bytevector(TrickType):
    serialization_id = 12

    class Iterator:
        def __init__(self, bvector):
            self._bvector = bvector
            self._i = 0

        def __next__(self):
            if self._i >= len(self._bvector):
                raise StopIteration

            value = self._bvector[self._i]
            self._i += 1
            return value

    def __init__(self, elements: list[Integer]):
        self.bytes = bytes(elements)

    def __iter__(self):
        return Bytevector.Iterator(self)

    def __getitem__(self, idx: int):
        return Integer(self.bytes[idx])

    def __setitem__(self, idx: int, value: Integer):
        if value < 0 or value > 255:
            raise ValueError('Invalid element value for bytevector')
        self.bytes = self.bytes[:idx] + bytes([value]) + self.bytes[idx+1:]

    def to_list(self) -> list[Integer]:
        return [Integer(i) for i in self.bytes]

    def __len__(self):
        return len(self.bytes)

    def __repr__(self):
        return f'#u8({" ".join(str(i) for i in self.bytes)})'

    def _dump(self, output):
        self._dump_list([Integer(i) for i in self.bytes], output)

    @classmethod
    def _load(cls, input):
        return Bytevector(cls._load_list(Integer, input))


class Port(TrickType):
    def __init__(self, file, mode: str, dir: str, *, filename=None):
        assert mode in ['text', 'binary']
        assert dir in ['input', 'output']
        assert filename is None or isinstance(filename, str)
        self.file = file
        self.mode = mode
        self.dir = dir
        self.filename = filename

    def __repr__(self):
        if self.filename:
            return f'#<{self.mode} port "{self.filename}">'
        else:
            return f'#<{self.mode} port>'

    def is_text(self):
        return self.mode == 'text'

    def is_binary(self):
        return self.mode == 'binary'

    def is_input(self):
        return self.dir == 'input'

    def is_output(self):
        return self.dir == 'output'


class WrappedValue(TrickType):
    def __init__(self, value: TrickType, type_id: Symbol):
        self.value = value
        self.type_id = type_id

    def __str__(self):
        return f'#<wrapped {self.type_id} {self.value}>'


shareable_types = (Pair, Vector)
