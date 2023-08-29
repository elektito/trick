from fractions import Fraction
import io
from machinetypes import Bytevector, Complex, Float, Integer, Rational, Symbol, List, Nil, Pair, Bool, String, Char, Reference, TrickType, Vector


class ReadError(Exception):
    pass


class Label:
    def __init__(self, value):
        self.value = value

    def __hash__(self):
        return hash(self.value)

    def __eq__(self, other):
        if not isinstance(other, Label):
            return False
        return self.value == other.value

    def __repr__(self):
        return f'<Datum Label #={self.value}>'


class Reader:
    def __init__(self, input, *, casefold=False):
        assert hasattr(input, 'read')

        self._input = input
        self.input_idx = 0
        self._unread_chars = []
        self._fold_case = casefold
        self._labeled_data = {}
        self._seen_labels = set()

    def read(self) -> (None | TrickType):
        """
        Return None if EOF, otherwise a Scheme object is read and returned.
        """
        self._labeled_data = {}
        self._seen_labels = set()

        value = self._read()
        if value == Symbol('.'):
            raise ReadError('Unexpected dot (.)')

        self._resolve_refs(value)

        return value

    def read_all(self) -> list[TrickType]:
        """Read all expressions in the input file and return them as a python
        list."""
        forms = []
        while True:
            form = self.read()
            if form is None:
                break
            forms.append(form)
        return forms

    def _read(self, eof_error=None, allow_delim=None, label=None) -> (None | TrickType):
        self._skip_whitespace(eof_error)

        start = self.input_idx

        cur_char = self._read_one_char(eof_error)
        if not cur_char:
            if eof_error:
                raise ReadError(eof_error)
            return None

        match cur_char:
            case '(':
                value = self._read_list(delim=')')

            case '[':
                value = self._read_list(delim=']')

            case ')':
                if allow_delim == ')':
                    value = None
                else:
                    raise ReadError('Unbalanced parentheses')

            case ']':
                if allow_delim == ']':
                    value = None
                else:
                    raise ReadError('Unbalanced parentheses')

            case '"':
                value = self._read_string()

            case "'":
                quoted = self._read(eof_error='Unexpected end-of-file reading quote')
                value = Pair(Symbol('quote'), Pair(quoted, Nil()))

            case '`':
                quoted = self._read(eof_error='Unexpected end-of-file reading quasiquote')
                value = Pair(Symbol('quasiquote'), Pair(quoted, Nil()))

            case ',':
                value = self._read_unquote(eof_error='Unexpected end-of-file reading unquote')

            case '#':
                value = self._read_sharp_value()
                if value == Symbol('#t'):
                    value = Bool(True)
                elif value == Symbol('#f'):
                    value = Bool(False)

            case ';':
                self._skip_semicolon_comment()
                value = self._read(label=label, eof_error=eof_error, allow_delim=allow_delim)

            case _:
                token = self._read_token(start_char=cur_char)
                value = parse_token(token, case_fold=self._fold_case)

        if value is not None:
            value.src_start = start
            value.src_end = self.input_idx

        if label is not None:
            self._labeled_data[label.value] = value

        return value

    def _read_one_char(self, eof_error=None):
        if self._unread_chars:
            c = self._unread_chars.pop()
            self.input_idx += 1
            return c

        c = self._input.read(1)
        if c == '' and eof_error:
            raise ReadError(eof_error)
        if c != '':
            self.input_idx += 1
        return c

    def _unread_one_char(self, char):
        self._unread_chars.insert(0, char)
        self.input_idx -= 1

    def _skip_whitespace(self, eof_error=None) -> (None | str):
        while True:
            cur_char = self._read_one_char(eof_error)
            if not cur_char:
                break
            elif cur_char == '#':
                next_char = self._read_one_char(eof_error)
                if next_char == '|':
                    self._skip_block_comment()
                elif next_char == ';':
                    self._read(eof_error=eof_error)
                elif next_char == '!':
                    directives = ['fold-case', 'no-fold-case']
                    first_char = self._read_one_char('End-of-file in "#!" directive')
                    token = self._read_token(first_char)
                    if token in directives:
                        self._perform_directive(token)
                    else:
                        raise ReadError(f'Invalid directive: #!{token}')
                else:
                    self._unread_one_char(cur_char)
                    self._unread_one_char(next_char)
                    break
            elif cur_char == ';':
                self._skip_semicolon_comment()
            elif cur_char.isspace():
                pass
            else:
                self._unread_one_char(cur_char)
                break

    def _skip_semicolon_comment(self, eof_error=None):
        cur_char = self._read_one_char(eof_error)
        while cur_char and cur_char != '\n':
            cur_char = self._read_one_char(eof_error)

        if cur_char:
            if eof_error:
                raise ReadError(eof_error)
            return None
        else:
            self._unread_one_char(cur_char)

    def _read_vector(self):
        ls = self._read_list(
            delim=')',
            no_dot_allowed=True)
        return Vector(ls)

    def _read_bytevector(self):
        # from the prefix "#u8", the first two characters are already read.
        c = self._read_one_char()
        if c != '8':
            raise ReadError(f'Expected "8" after "#u", got: {c}')

        c = self._read_one_char()
        if c != '(':
            raise ReadError(f'Expected "(" after "#u8(", got: {c}')

        ls = self._read_list(delim=')', no_dot_allowed=True)
        for e in ls:
            if not isinstance(e, Integer) or e < 0 or e > 255:
                raise ReadError(
                    f'Bytevector elements must be integers in range '
                    f'0-255; got: {e}')

        return Bytevector(ls.to_list())

    def _read_list(self, delim, no_dot_allowed=False):
        read_dot = False
        values = []
        value_after_dot = None
        while True:
            value = self._read(allow_delim=delim, eof_error='List not closed')
            if value is None:
                if read_dot and value_after_dot is None:
                    raise ReadError('No item after dot')
                elif read_dot:
                    result = List.from_list(values, after_dot=value_after_dot)
                    return result
                else:
                    return List.from_list(values)

            if value == Symbol('.'):
                if no_dot_allowed:
                    raise ReadError('Misplaced dot')
                if values == []:
                    raise ReadError('Unexpected dot (.) at the start of list')
                if read_dot:
                    raise ReadError('More than one dot in list')
                read_dot = True
            elif read_dot:
                if value_after_dot is not None:
                    raise ReadError('More than one item after dot')
                value_after_dot = value
            else:
                values.append(value)

    def _read_string(self, delim='"'):
        eof_error = 'Unexpected end-of-file while reading string'
        read_str = ''
        while True:
            cur_char = self._read_one_char(eof_error)
            if cur_char == '\\':
                escape = self._read_one_char(eof_error)
                map = {
                    'a': '\a',
                    'b': '\b',
                    't': '\t',
                    'n': '\n',
                    'r': '\r',
                    '"': '"',
                    '\\': '\\',
                    '|': '|',
                }
                if escape in map:
                    read_str += map[escape]
                elif escape in ' \n':
                    # skip an optional number of spaces, a newline, and another
                    # optional number of spaces.
                    found_newline = (escape == '\n')
                    while True:
                        cur_char = self._read_one_char(eof_error)
                        if cur_char == ' ':
                            continue
                        elif cur_char == '\n' and not found_newline:
                            found_newline = True
                        else:
                            self._unread_one_char(cur_char)
                            break
                    if not found_newline:
                        raise ReadError('No newline after "\\ "')
                elif escape == 'x':
                    hex_number = ''
                    while not hex_number.endswith(';'):
                        hex_number += self._read_one_char(eof_error)
                    try:
                        char_code = int(hex_number[:-1], 16)
                    except ValueError:
                        raise ReadError(
                            f'Invalid string hex escape number: "{hex_number}"')
                    try:
                        char = chr(char_code)
                    except (ValueError, OverflowError):
                        raise ReadError(
                            f'Invalid character code in string hex escape: '
                            f'{hex(char_code)[2:]}')
                    read_str += char
                else:
                    raise ReadError(f'Invalid escape character: "{escape}"')
            elif cur_char == delim:
                return String(read_str)
            else:
                read_str += cur_char

    def _read_unquote(self, eof_error=None):
        c = self._read_one_char(eof_error)
        if c == '@':
            unquoted = self._read(eof_error='End-of-file after ",@"')
            return Pair(Symbol('unquote-splicing'), Pair(unquoted, Nil()))
        else:
            self._unread_one_char(c)
            unquoted = self._read(eof_error='End-of-file after ","')
            return Pair(Symbol('unquote'), Pair(unquoted, Nil()))

    def _read_token(self, start_char, eof_error=None):
        if start_char == '|':
            string = self._read_string(delim='|')
            return '|' + string.value + '|'

        token = start_char
        cur_char = start_char
        while True:
            cur_char = self._read_one_char(eof_error)
            if not cur_char or self._is_separator(cur_char):
                break
            if cur_char:
                token += cur_char

        if cur_char:
            self._unread_one_char(cur_char)

        return token

    def _is_separator(self, char):
        return char.isspace() or char in "()[]'`"

    def _read_sharp_value(self, eof_error=None):
        char = self._read_one_char(eof_error='Invalid sharp sign at end-of-file')
        if char == 'x': #x (hex literal)
            first_char = self._read_one_char('End-of-file in hex literal')
            token = self._read_token(first_char)
            return parse_token('#x' + token, case_fold=self._fold_case)
        elif char == 'o': #o (octal literal)
            first_char = self._read_one_char('End-of-file in octal literal')
            token = self._read_token(first_char)
            return parse_token('#o' + token, case_fold=self._fold_case)
        elif char == 'b': #b (binary literal)
            first_char = self._read_one_char('End-of-file in binary literal')
            token = self._read_token(first_char)
            return parse_token('#b' + token, case_fold=self._fold_case)
        elif char == 'd': #d (decimal literal with explicit prefix)
            first_char = self._read_one_char('End-of-file in decimal literal')
            token = self._read_token(first_char)
            return parse_token('#d' + token, case_fold=self._fold_case)
        elif char == 'e': #e (exact number)
            first_char = self._read_one_char('End-of-file in exact literal')
            token = self._read_token(first_char)
            return parse_token('#e' + token, case_fold=self._fold_case)
        elif char == 'i': #i (inexact number)
            first_char = self._read_one_char('End-of-file in inexact literal')
            token = self._read_token(first_char)
            return parse_token('#i' + token, case_fold=self._fold_case)
        elif char.isnumeric(): #<n>= or #<n># (label or reference)
            value = self._read_label_or_reference(start_char=char)
            if isinstance(value, Label):
                return self._read(eof_error=eof_error, label=value)
            elif isinstance(value, Reference):
                return value
            else:
                assert False
        elif char == '\\': #\ (character literal)
            first_char = self._read_one_char(
                eof_error='End-of-file while reading character literal')
            desc = self._read_token(first_char)
            if desc in Char.name_to_code:
                return Char(Char.name_to_code[desc])
            else:
                if len(desc) == 1:
                    char_code = ord(desc)
                elif desc.startswith('x'):
                    try:
                        char_code = int(desc[1:], 16)
                    except ValueError:
                        raise ReadError(f'Invalid character literal: #\\{desc}')
                else:
                    raise ReadError(f'Invalid character literal: #\\{desc}')
                return Char(char_code)
        elif char == '(':  # vector
            return self._read_vector()
        elif char == 'u':
            return self._read_bytevector()
        else: # special symbol
            token = self._read_token(start_char=char)
            return Symbol('#' + token)

    def _skip_block_comment(self):
        while True:
            char = self._read_one_char(eof_error='End-of-file in block comment')
            if char == '|':
                char = self._read_one_char(eof_error='End-of-file in block comment')
                if char == '#':
                    return
                else:
                    self._unread_one_char(char)
            elif char == '#':
                char = self._read_one_char(eof_error='End-of-file in block comment')
                if char == '|':
                    self._skip_block_comment()
                else:
                    self._unread_one_char(char)

    def _resolve_refs(self, value):
        if isinstance(value, Reference):
            refs = [value]
            while isinstance(value, Reference):
                try:
                    value = self._labeled_data[value.label]
                except KeyError:
                    raise ReadError(f'Unknown datum label: #{value.label}#')
                if value in refs:
                    # something like #0=#0#, or #0=#1=#0#
                    raise ReadError(f'Circular datum reference')
                elif isinstance(value, Reference):
                    refs.append(value)
            return value
        elif isinstance(value, Pair):
            cur = value
            while True:
                cur.car = self._resolve_refs(cur.car)
                if not isinstance(cur.cdr, Pair):
                    cur.cdr = self._resolve_refs(cur.cdr)
                    break
                else:
                    cur = cur.cdr

            return value
        elif isinstance(value, Vector):
            for i in range(len(value)):
                value[i] = self._resolve_refs(value[i])
            return value
        else:
            return value

    def _read_label_or_reference(self, start_char: str, eof_error=None):
        number = start_char
        while True:
            char = self._read_one_char(eof_error=f'Invalid label/reference: #{number}')
            if char == '#':
                return Reference(int(number))
            elif char == '=':
                label = Label(int(number))
                if label in self._seen_labels:
                    raise ReadError(f'Duplicate datum label: #{number}=')
                self._seen_labels.add(label)
                return label
            elif char.isnumeric():
                number += char
            else:
                raise ReadError(f'Invalid label/reference: #{number}{char}')

    def _perform_directive(self, directive):
        if directive == 'fold-case':
            self._fold_case = True
        elif directive == 'no-fold-case':
            self._fold_case = False
        else:
            assert False


def parse_token(token: str, *, case_fold):
    """
    parse the given string as a number if possible, otherwise as a
    symbol. might raise ReadError.
    """
    n = parse_number(token)
    if n is not None:
        return n

    # if token.lower() == '+inf.0':
    #     value = Float('+inf')
    # elif token.lower() == '-inf.0':
    #     value = Float('-inf')
    # elif token.lower() == '+nan.0':
    #     value = Float('nan')
    # elif token.lower() == '-nan.0':
    #     value = Float('nan')
    # else:

    if case_fold:
        token = token.casefold()
    if token.startswith('|') and token.endswith('|'):
        token = token[1:-1]
    value = Symbol(token)

    return value


def parse_number(token: str):
    """
    parse the given number as a number. if not possible, return None.
    """

    force = None
    base = 10

    base_prefixes = {
        '#x': 16,
        '#d': 10,
        '#o': 8,
        '#b': 2,
    }

    if token[:2] in ['#e', '#i']:
        force = 'exact' if token[:2] == '#e' else 'inexact'
        token = token[2:]
        if token[:2] in base_prefixes:
            base = base_prefixes[token[:2]]
            token = token[2:]
    elif token[:2] in base_prefixes:
        base = base_prefixes[token[:2]]
        token = token[2:]
        if token[:2] in ['#e', '#i']:
            force = 'exact' if token[:2] == '#e' else 'inexact'
            token = token[2:]

    return parse_complex(token, base, force)


def parse_complex(token: str, base: int, force: (str|None)):
    if token.endswith('i'):
        return parse_strict_complex(token, base, force)
    else:
        return parse_real(token, base, force)


def parse_real(token: str, base: int, force: (str|None)):
    if '/' in token:
        return parse_rational(token, base, force)
    elif any(c in token for c in '.ed'): # d and e for scientific notation
        return parse_float(token, base, force)
    else:
        return parse_integer(token, base, force)


def parse_integer(token: str, base: int, force: (str|None)):
    try:
        n = int(token, base)
    except ValueError:
        return None

    if force == 'inexact':
        return Float(n)
    else:
        return Integer(n)


def parse_float(token: str, base: int, force: (str|None)):
    if token in ['+inf.0', '-inf.0', '+nan.0', '-nan.0']:
        return Float(token[:-2])

    if base != 10:
        return None

    try:
        n = float(token)
    except ValueError:
        return None

    if force == 'exact':
        return Rational(Fraction(n))
    else:
        return Float(n)


def parse_rational(token: str, base: int, force: (str|None)):
    parts = token.split('/')
    if len(parts) != 2:
        return None

    num, den = parts

    try:
        num = int(num, base)
        den = int(den, base)
    except ValueError:
        return None

    n = Fraction(num, den)

    if force == 'inexact':
        return Float(n)
    else:
        return Rational(n)


def parse_strict_complex(token: str, base: int, force: (str|None)):
    if token == '+i':
        return Complex(Integer(0), Integer(1))
    elif token == '-i':
        return Complex(Integer(0), Integer(-1))
    elif token in ['+inf.0i', '-inf.0i', '+nan.0i', '-nan.0i']:
        return Complex(Integer(0), Float(token[:-3]))
    else:
        real_sign = 1
        if token.startswith('+'):
            token = token[1:]
        elif token.startswith('-'):
            real_sign = -1
            token = token[1:]

        if '+' in token:
            imag_sign = '+'
            parts = token.split('+')
        elif '-' in token:
            imag_sign = '-'
            parts = token.split('-')
        else: # e.g. 9i
            imag = parse_real(token[:-1], base, force)
            if imag is None:
                return None
            return Complex(Integer(0), imag)

        if len(parts) != 2:
            return None

        real, imaginary = parts

        # strip "i" suffix
        if imaginary == 'i':
            imaginary = '1'
        elif imaginary == '-i':
            imaginary = '-1'
        else:
            imaginary = imaginary[:-1]

        imaginary = imag_sign + imaginary

        real = parse_real(real, base, force)
        imaginary = parse_real(imaginary, base, force)
        if real is None or imaginary is None:
            return None

        # if there's no force (already taken into account in parsing real and
        # imaginary), we'll convert both to float if one of them is.
        if isinstance(real, Float) and not isinstance(imaginary, Float):
            if isinstance(imaginary, Rational):
                imaginary = Float(imaginary.frac)
            else:
                imaginary = Float(imaginary)
        if isinstance(imaginary, Float) and not isinstance(real, Float):
            if isinstance(real, Rational):
                real = Float(real.frac)
            else:
                real = Float(float(real))

        real *= real_sign

        return Complex(real, imaginary)


def read_expr(text):
    input = io.StringIO(text)
    reader = Reader(input)
    expr = reader.read()

    try:
        trailer = reader.read()
    except ReadError:
        raise ReadError('Buffer contains more than a single expression')
    if trailer is not None:
        ReadError('Buffer contains more than a single expression')

    return expr
