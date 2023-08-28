import inspect
import io
import os
import sys
import traceback

from exceptions import RunError
from machinetypes import Bool, Bytevector, Integer, Port, String, Symbol, TrickType, Void
from print import PrintMode, PrintStyle, Printer


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


@module(opcode=0x03)
class Sys(RuntimeModule):
    @proc(opcode=0x01)
    def exit(self, exit_code: Integer) -> Void:
        raise TrickExitException(self, 'exit', exit_code)


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
