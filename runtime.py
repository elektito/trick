import inspect
import sys
import traceback

from machinetypes import Bool, Integer, Port, String, Symbol, TrickType
from print import PrintMode, PrintStyle, Printer
from secd import RunError, Secd


modules = {}
_proc_methods = []


class TrickRuntimeError(RunError):
    def __init__(self, module, proc_name, msg):
        self.module = module
        self.module_name = type(module).__name__.lower()
        self.proc_name = proc_name
        self.msg = msg

    def __str__(self):
        return f'{self.module_name}/{self.proc_name}: {self.msg}'


class RuntimeModule:
    def _runtime_error(self, msg):
        # get the name of the function that called _runtime_error
        stack = traceback.extract_stack()
        proc_name = stack[-2].name

        return TrickRuntimeError(self, proc_name, msg)


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
        self._stdin = Port(sys.stdin, 'text', filename='<stdin>')
        self._stdout = Port(sys.stdout, 'text', filename='<stdout>')
        self._stderr = Port(sys.stderr, 'text', filename='<stderr>')

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
        port.write(text)
        return Bool(True)

    @proc(opcode=0x05)
    def flush(self, port: Port) -> TrickType:
        port.file.flush()
        return Bool(True)


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
