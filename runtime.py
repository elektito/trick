import cmath
from fractions import Fraction
import inspect
import io
import math
import os
import sys
import traceback

from exceptions import RunError
from machinetypes import Bool, Bytevector, Complex, Float, Integer, Number, Port, Rational, String, Symbol, TrickType, Void
from print import PrintMode, PrintStyle, Printer
from read import ReadError, Reader


modules = {}
_proc_methods = []


class TrickRuntimeError(RunError):
    def __init__(self, module, proc_name, msg, *, kind=None):
        self.module = module
        self.module_name = type(module).__name__.lower()
        self.proc_name = proc_name
        self.msg = msg
        self.kind = kind

    def __str__(self):
        return f'{self.module_name}/{self.proc_name}: {self.msg}'


class TrickExitException(TrickRuntimeError):
    def __init__(self, module, proc_name, exit_code):
        super().__init__(module, proc_name, f'Exit with code {exit_code}')
        self.exit_code = exit_code

    def __str__(self):
        return f'<TrickExitException exit_code={self.exit_code}>'


class RuntimeModule:
    def _runtime_error(self, msg, kind=None):
        # get the name of the function that called _runtime_error
        stack = traceback.extract_stack()
        proc_name = stack[-2].name

        return TrickRuntimeError(self, proc_name, msg, kind=kind)

    def _file_error(self, msg):
        return self._runtime_error(msg, kind=Symbol('file'))


def module(opcode):
    def decorator(klass):
        name = klass.__name__.lower()
        modules[name] = {
            'class': klass,
            'opcode': opcode,
            'procs': {},
        }
        return klass
    return decorator


def proc(*, opcode):
    def decorator(func):
        name = func.__name__.lower()
        _proc_methods.append((func, opcode, name))
        return func
    return decorator


@module(opcode=0x01)
class Io(RuntimeModule):
    def __init__(self):
        self._stdin = Port(sys.stdin, 'text', 'input', filename='<stdin>')
        self._stdout = Port(sys.stdout, 'text', 'output', filename='<stdout>')
        self._stderr = Port(sys.stderr, 'text', 'output', filename='<stderr>')

    @proc(opcode=0x01)
    def stdin(self) -> Port:
        return self._stdin

    @proc(opcode=0x02)
    def stdout(self) -> Port:
        return self._stdout

    @proc(opcode=0x03)
    def stderr(self) -> Port:
        return self._stderr

    @proc(opcode=0x04)
    def write(self, text: String, port: Port) -> TrickType:
        # writing on a closed file doesn't throw OSError (but ValueError), so
        # let's check for closed files first.
        if port.file.closed:
            raise self._file_error(
                f'Cannot write to closed port: {port}')

        if not port.is_text():
            raise self._file_error(
                f'Cannot write text to binary port: {port}')

        try:
            port.file.write(text.value)
        except OSError as e:
            raise self._file_error(str(e))

        return Void()

    @proc(opcode=0x05)
    def flush(self, port: Port) -> TrickType:
        # operating on a closed file doesn't throw OSError (but ValueError), so
        # let's check for closed files first.
        if port.file.closed:
            raise self._file_error(
                f'Cannot flush closed port: {port}')

        try:
            port.file.flush()
        except OSError as e:
            raise self._file_error(str(e))

        return Void()

    @proc(opcode=0x06)
    def open(self, filename: String, mode: String) -> Port:
        try:
            file = open(filename.value, mode.value)
        except OSError as e:
            raise self._file_error(str(e))

        return Port(
            file=file,
            mode='text' if mode.value in ('r', 'w') else 'binary',
            dir='input' if mode.value in ('r', 'rb') else 'output',
            filename=filename.value,
        )

    @proc(opcode=0x07)
    def read(self, port: Port, n: Integer) -> String:
        # reading from closed file doesn't throw OSError (but ValueError), so
        # let's check for closed files first.
        if port.file.closed:
            raise self._file_error(
                f'Cannot read from closed port: {port}')

        if not port.is_text():
            raise self._file_error(
                f'Cannot read text from binary file: {port}')

        try:
            s = port.file.read(n)
        except OSError as e:
            raise self._file_error(str(e))

        return String(s)

    @proc(opcode=0x08)
    def portmode(self, port: Port) -> Symbol:
        if port.is_text():
            return Symbol('text')
        else:
            return Symbol('binary')

    @proc(opcode=0x09)
    def portdir(self, port: Port) -> Symbol:
        if port.is_input():
            return Symbol('input')
        else:
            return Symbol('output')

    @proc(opcode=0x0a)
    def close(self, port: Port) -> Void:
        try:
            port.file.close()
        except OSError as e:
            raise self._file_error(str(e))

        return Void()

    @proc(opcode=0x0b)
    def readbyte(self, port: Port) -> Integer:
        # reading from closed file doesn't throw OSError (but ValueError), so
        # let's check for closed files first.
        if port.file.closed:
            raise self._file_error(
                f'Cannot read from closed port: {port}')

        if not port.is_binary():
            raise self._file_error(
                f'Cannot read a byte from textual port: {port}')

        try:
            s = port.file.read(1)
        except OSError as e:
            raise self._file_error(str(e))

        if len(s) == 0:
            # EOF
            return Integer(-1)

        return Integer(s[0])

    @proc(opcode=0x0c)
    def readbv(self, port: Port, n: Integer) -> Bytevector:
        # reading from closed file doesn't throw OSError (but ValueError), so
        # let's check for closed files first.
        if port.file.closed:
            raise self._file_error(
                f'Cannot read from closed port: {port}')

        if not port.is_binary():
            raise self._file_error(
                f'Cannot read bytes from textual file: {port}')

        try:
            s = port.file.read(n)
        except OSError as e:
            raise self._file_error(str(e))

        return Bytevector([Integer(i) for i in s])

    @proc(opcode=0x0d)
    def readline(self, port: Port) -> String:
        # reading from closed file doesn't throw OSError (but ValueError), so
        # let's check for closed files first.
        if port.file.closed:
            raise self._file_error(
                f'Cannot read from closed port: {port}')

        if not port.is_text():
            raise self._file_error(
                f'Cannot read text from binary file: {port}')

        try:
            s = port.file.readline()
        except OSError as e:
            raise self._file_error(str(e))

        return String(s)

    @proc(opcode=0x0e)
    def openistr(self, s: String) -> Port:
        file = io.StringIO(s.value)
        return Port(
            file=file,
            mode='text',
            dir='input',
            filename=None,
        )

    @proc(opcode=0x0f)
    def openibv(self, bv: Bytevector) -> Port:
        file = io.BytesIO(bv.bytes)
        return Port(
            file=file,
            mode='binary',
            dir='input',
            filename=None,
        )

    @proc(opcode=0x10)
    def openostr(self) -> Port:
        file = io.StringIO()
        return Port(
            file=file,
            mode='text',
            dir='output',
            filename=None,
        )

    @proc(opcode=0x11)
    def openobv(self) -> Port:
        file = io.BytesIO()
        return Port(
            file=file,
            mode='binary',
            dir='output',
            filename=None,
        )

    @proc(opcode=0x12)
    def portstr(self, port: Port) -> String:
        if not isinstance(port.file, io.StringIO):
            raise self._runtime_error(
                f'Port not created using open-output-string: {port}')

        return String(port.file.getvalue())

    @proc(opcode=0x13)
    def portbv(self, port: Port) -> Bytevector:
        if not isinstance(port.file, io.BytesIO):
            raise self._runtime_error(
                f'Port not created using open-output-string: {port}')

        return Bytevector([Integer(i) for i in port.file.getvalue()])

    @proc(opcode=0x14)
    def writebv(self, bv: Bytevector, port: Port) -> TrickType:
        # writing on a closed file doesn't throw OSError (but ValueError), so
        # let's check for closed files first.
        if port.file.closed:
            raise self._file_error(
                f'Cannot write to closed port: {port}')

        if not port.is_binary():
            raise self._file_error(
                f'Cannot write bytes to textual port: {port}')

        try:
            port.file.write(bv.bytes)
        except OSError as e:
            raise self._file_error(str(e))

        return Void()

    @proc(opcode=0x15)
    def isopen(self, port: Port) -> Bool:
        return Bool(not port.file.closed)

    @proc(opcode=0x16)
    def exists(self, filename: String) -> Bool:
        return Bool(os.path.exists(filename.value))

    @proc(opcode=0x17)
    def delete(self, filename: String) -> Void:
        try:
            os.unlink(filename.value)
        except OSError as e:
            raise self._file_error(str(e))

        return Void()


@module(opcode=0x02)
class Str(RuntimeModule):
    @proc(opcode=0x01)
    def format(self, mode: Symbol, style: Symbol, obj: TrickType) -> String:
        mode_enum = {
            'simple': PrintMode.Simple,
            'cyclic': PrintMode.Cyclic,
            'shared': PrintMode.Shared,
        }.get(mode.name)
        if mode_enum is None:
            raise self._runtime_error(f'Invalid print mode: {mode}')

        style_enum = {
            'write': PrintStyle.Write,
            'display': PrintStyle.Display,
        }.get(style.name)
        if style_enum is None:
            raise self._runtime_error(f'Invalid print style: {style}')

        printer = Printer(obj, mode=mode_enum, style=style_enum)
        string = printer.print()

        return String(string)

    @proc(opcode=0x02)
    def fromutf8(self, bv: Bytevector, start: Integer, end: Integer) -> String:
        if start < 0 or end > len(bv):
            raise self._runtime_error(
                f'Invalid bytevector range: {start}-{end}')

        try:
            s = bv.bytes[start:end].decode('utf-8')
        except UnicodeDecodeError as e:
            raise self._runtime_error(str(e), kind=Symbol('unicode'))

        return String(s)

    @proc(opcode=0x03)
    def toutf8(self, s: String, start: Integer, end: Integer) -> Bytevector:
        if start < 0 or end > len(s):
            raise self._runtime_error(
                f'Invalid bytevector range: {start}-{end}')

        try:
            bv = s.value[start:end].encode('utf-8')
        except UnicodeEncodeError as e:
            raise self._runtime_error(str(e), kind=Symbol('unicode'))

        return Bytevector([Integer(b) for b in bv])

    @proc(opcode=0x04)
    def fmtnum(self, z: Number, base: Integer) -> String:
        digit_map = {
            0: '0',
            1: '1',
            2: '2',
            3: '3',
            4: '4',
            5: '5',
            6: '6',
            7: '7',
            8: '8',
            9: '9',
            10: 'a',
            11: 'b',
            12: 'c',
            13: 'd',
            14: 'e',
            15: 'f',
            16: 'g',
            17: 'h',
            18: 'i',
            19: 'j',
            20: 'k',
            21: 'l',
            22: 'm',
            23: 'n',
            24: 'o',
            25: 'p',
            26: 'q',
            27: 'r',
            28: 's',
            29: 't',
            30: 'u',
            31: 'v',
            32: 'w',
            33: 'x',
            34: 'y',
            35: 'z',
        }

        def int_to_base(n: int, base: int) -> str:
            n = int(n)
            s = ''
            if n < 0:
                s = '-'
                n = -n
            while n > 0:
                digit = n % base
                digit = digit_map[digit]
                s = digit + s
                n //= base
            return s

        if base < 1 or base > 36:
            raise self._runtime_error('Only bases 2-36 supported')

        if isinstance(z, Complex):
            sign = '+' if z.imag >= 0 else ''
            real = self.fmtnum(z.real, base).value
            imag = self.fmtnum(z.imag, base).value
            return String(real + sign + imag + 'i')
        else:
            if z.exact:
                if isinstance(z, Integer):
                    return String(int_to_base(z, base))
                elif isinstance(z, Rational):
                    num = int_to_base(z.frac.numerator, base)
                    den = int_to_base(z.frac.denominator, base)
                    return String(num + '/' + den)
                else:
                    assert False, 'unhandled case'
            else:
                # r7rs number->string description says that if the number is
                # inexact, the radix is always 10.
                return String(str(z))

    @proc(opcode=0x05)
    def cmp(self, s1: String, s2: String, options: Integer) -> Integer:
        case_insensitive = False
        if options != 0:
            case_insensitive = True

        s1 = s1.value
        s2 = s2.value

        if case_insensitive:
            s1 = s1.casefold()
            s2 = s2.casefold()

        if s1 == s2:
            result = 0
        elif s1 < s2:
            result = -1
        elif s1 > s2:
            result = 1

        return Integer(result)


@module(opcode=0x03)
class Sys(RuntimeModule):
    @proc(opcode=0x01)
    def exit(self, exit_code: Integer) -> Void:
        raise TrickExitException(self, 'exit', exit_code)


@module(opcode=0x04)
class Read(RuntimeModule):
    @proc(opcode=0x01)
    def read(self, port: Port) -> TrickType:
        # reading from closed file doesn't throw OSError (but ValueError), so
        # let's check for closed files first.
        if port.file.closed:
            raise self._file_error(
                f'Cannot read from closed port: {port}')

        if not port.is_text():
            raise self._file_error(
                f'Cannot read text from binary file: {port}')

        try:
            reader = Reader(port.file)
            result = reader.read()
        except ReadError as e:
            raise self._runtime_error(str(e), kind=Symbol('read'))
        except OSError as e:
            raise self._file_error(str(e))

        if result is None:
            return Void()

        return result


@module(opcode=0x05)
class Math(RuntimeModule):
    def _from_py(self, z: complex) -> Number:
        r = Number.from_python_number(z)
        r = r.to_specific()
        return r

    def _to_py(self, z: Number) -> complex:
        r = z.to_complex()
        real = r.real.to_float()
        imag = r.imag.to_float()
        return complex(real, imag)

    def _do_math(self, z: Number, real_func, complex_func):
        if isinstance(z, Complex):
            return self._from_py(complex_func(self._to_py(z)))
        else:
            nz = z.to_python_number()
            try:
                r = real_func(nz)
            except ValueError:  # math domain error
                r = complex_func(nz)
            return Number.from_python_number(r)

    @proc(opcode=0x01)
    def exp(self, z: Number) -> Number:
        return self._do_math(z, math.exp, cmath.exp)

    @proc(opcode=0x02)
    def ln(self, z: Number) -> Number:
        if z.is_negative_zero():
            # see section 6.2.6, under "log"
            return self._from_py(float('-inf') + math.pi * 1j)
        elif z.is_zero():
            # see section 6.2.6, under "log"
            return Float('-inf')
        elif z == Complex(Float(-1.0), Float(-0.0)):
            # see section 6.2.4
            return Complex(Float(0.0), Float(-math.pi))

        return self._do_math(z, math.log, cmath.log)

    @proc(opcode=0x03)
    def log(self, z: Number, base: Number) -> Number:
        if z.is_negative_zero():
            # see section 6.2.6, under "log"
            return self._from_py(float('-inf') + math.pi * 1j)
        elif z.is_zero():
            # see section 6.2.6, under "log"
            return Float('-inf')
        elif z == Complex(Float(-1.0), Float(-0.0)):
            # see section 6.2.4
            return Complex(Float(0.0), Float(-math.pi))

        if isinstance(z, Complex) or isinstance(base, Complex):
            return self._from_py(cmath.log(self._to_py(z), self._to_py(base)))
        else:
            nz = z.to_python_number()
            nbase = base.to_python_number()
            try:
                r = math.log(nz, nbase)
            except ValueError:  # math domain error
                r = cmath.log(nz, nbase)
            return Number.from_python_number(r)

    @proc(opcode=0x04)
    def sin(self, z: Number) -> Number:
        return self._do_math(z, math.sin, cmath.sin)

    @proc(opcode=0x05)
    def cos(self, z: Number) -> Number:
        return self._do_math(z, math.cos, cmath.cos)

    @proc(opcode=0x06)
    def tan(self, z: Number) -> Number:
        return self._do_math(z, math.tan, cmath.tan)

    @proc(opcode=0x07)
    def asin(self, z: Number) -> Number:
        return self._do_math(z, math.asin, cmath.asin)

    @proc(opcode=0x08)
    def acos(self, z: Number) -> Number:
        return self._do_math(z, math.acos, cmath.acos)

    @proc(opcode=0x09)
    def atan(self, z: Number) -> Number:
        return self._do_math(z, math.atan, cmath.atan)

    @proc(opcode=0x0a)
    def atan2(self, x: Number, y: Number) -> Float:
        try:
            x = x.to_float()
            y = y.to_float()
        except ValueError:
            raise self._runtime_error(f'Cannot calculate atan2 of: {x} and {y}')
        return Float(math.atan2(x, y))

    @proc(opcode=0x0b)
    def sqrt(self, z: Number) -> Number:
        # try to return an exact number, if the input is an exact number.
        if isinstance(z, Integer):
            pos = z if z >= 0 else -z
            r = math.sqrt(pos)
            if r.is_integer():
                r = Integer(r)
            else:
                r = Float(r)

            if z < 0:
                if isinstance(r, Float):
                    return Complex(Float(0), r)
                else:
                    return Complex(Integer(0), r)
            else:
                return r
        elif isinstance(z, Rational):
            pos = z if z >= 0 else -z
            num = math.sqrt(pos.frac.numerator)
            den = math.sqrt(pos.frac.denominator)
            if num.is_integer() and den.is_integer():
                r = Rational(Fraction(int(num), int(den)))
            else:
                r = Float(math.sqrt(z.frac))

            if z < 0:
                return Complex(Integer(0), r)
            else:
                return r
        elif isinstance(z, Complex) and z.exact:
            # See: https://math.stackexchange.com/questions/4760927/calculating-the-exact-square-root-of-a-complex-number-with-rational-components
            mag = self.sqrt(z.real * z.real + z.imag * z.imag)
            r_real = self.sqrt((z.real + mag) / Integer(2))
            r_imag = self.sqrt((-z.real + mag) / Integer(2))
            if isinstance(r_real, (Rational, Integer)) and isinstance(r_imag, (Rational, Integer)):
                r_imag *= z.imag / abs(z.imag)
                return Complex(r_real, r_imag)

        if isinstance(z, Complex):
            return self._from_py(cmath.sqrt(self._to_py(z)))
        else:
            assert isinstance(z, Float)
            if z < 0.0:
                return Complex(Float(0), Float(math.sqrt(-z)))
            else:
                return Float(math.sqrt(z))

    @proc(opcode=0x0c)
    def isnan(self, z: Number) -> Bool:
        if isinstance(z, Complex):
            return self.isnan(z.real) or self.isnan(z.imag)
        else:
            return Bool(math.isnan(z.to_python_number()))

    @proc(opcode=0x0d)
    def truncate(self, x: Number) -> Number:
        if isinstance(x, Complex):
            raise self._runtime_error(
                'Not a real number', kind=Symbol('math'))

        r = math.trunc(x.to_python_number())
        if x.exact:
            return Integer(r)
        else:
            return Float(r)

    @proc(opcode=0x0e)
    def floor(self, x: Number) -> Number:
        if isinstance(x, Complex):
            raise self._runtime_error(
                'Not a real number', kind=Symbol('math'))

        r = math.floor(x.to_python_number())
        if x.exact:
            return Integer(r)
        else:
            return Float(r)

    @proc(opcode=0x0f)
    def ceiling(self, x: Number) -> Number:
        if isinstance(x, Complex):
            raise self._runtime_error(
                'Not a real number', kind=Symbol('math'))

        r = math.ceil(x.to_python_number())
        if x.exact:
            return Integer(r)
        else:
            return Float(r)

    @proc(opcode=0x10)
    def round(self, x: Number) -> Number:
        if isinstance(x, Complex):
            raise self._runtime_error(
                'Not a real number', kind=Symbol('math'))

        r = round(x.to_python_number())
        if x.exact:
            return Integer(r)
        else:
            return Float(r)

    @proc(opcode=0x11)
    def expt(self, z1: Number, z2: Number) -> Number:
        if z1.is_zero() and z2.is_negative():
            raise self._runtime_error(
                f'expt is undefined for {z1} and {z2}')

        if isinstance(z1, Complex) or isinstance(z2, Complex):
            z1 = z1.to_complex()
            z2 = z2.to_complex()
            x = complex(z1.real.to_float(), z1.imag.to_float())
            y = complex(z2.real.to_float(), z2.imag.to_float())
            r = x ** y
            if r.imag == 0.0:
                return Float(r.real)
            else:
                return Complex(Float(r.real), Float(r.imag))
        else:
            x = z1.to_float()
            y = z2.to_float()
            r = x ** y
            return Float(r)


@module(opcode=0x99)
class Dbg(RuntimeModule):
    @proc(opcode=0x01)
    def print(self, obj: TrickType) -> TrickType:
        print(obj)
        return obj

    @proc(opcode=0x02)
    def format(self, obj: TrickType) -> String:
        p = Printer(obj)
        s = p.print()
        return String(s)


def find_proc(module_name, proc_name):
    return modules.get(module_name, {'procs': {}})['procs'].get(proc_name)


for func, opcode, name in _proc_methods:
    module_name, _ = func.__qualname__.split('.')
    module_name = module_name.lower()

    spec = inspect.getfullargspec(func)
    assert all(issubclass(t, TrickType) for t in spec.annotations.values())
    args = { # notice that we depend on the python dict to be ordered
        arg_name: spec.annotations[arg_name]
        for arg_name in spec.args
        if arg_name != 'self'
    }
    return_type = spec.annotations['return']

    if any(p['opcode'] == opcode for p in modules[module_name]['procs'].values()):
        raise ValueError(f'Duplicate proc opcode {opcode} in module {module_name}')

    modules[module_name]['procs'][name] = {
        'name': name,
        'module_opcode': modules[module_name]['opcode'],
        'opcode': opcode,
        'args': args,
        'return_type': return_type,
    }
