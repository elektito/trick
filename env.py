from copy import copy
from enum import Enum

from libloader import LibLoader
from machinetypes import Integer, Symbol
from serialization import Serializable
from symbolinfo import SymbolInfo, SymbolKind
from transform import Transformer
from utils import OrderedSet


class EnvironmentError(Exception):
    def __init__(self, msg: str, form=None, source=None):
        self.msg = msg
        self.form = form
        self.source = source

    def __repr__(self):
        return self.msg


class VariableKind(Serializable, Enum):
    NORMAL = 1
    UNHYGIENIC_MACRO = 2
    MACRO = 3

    def to_symbol_kind(self) -> SymbolKind:
        return {
            VariableKind.NORMAL: SymbolKind.DEFINED_NORMAL,
            VariableKind.UNHYGIENIC_MACRO: SymbolKind.DEFINED_UNHYGIENIC_MACRO,
            VariableKind.MACRO: SymbolKind.DEFINED_MACRO,
        }[self]

    def dump(self, output):
        self._dump_uint4(self.value, output)

    @classmethod
    def load(cls, input):
        value = cls._load_uint4(input)
        return VariableKind(value)


class Variable(Serializable):
    def __init__(self, name: Symbol, kind: VariableKind, value=None):
        # internal name of the variable; mangled inside librareis.
        self.name = name

        # The kind of variable (normal, macro, etc)
        self.kind = kind

        # the following are only meaningful for names defined at the top-level
        # using define, define-syntax, etc.

        # the value currently only stores the transformer for macros, otherwise
        # it's None.
        self.value = value

    def __repr__(self):
        return f'<Variable {self.name} ({self.kind.name})>'

    def dump(self, output):
        assert self.value is None or isinstance(self.value, Transformer)

        self.name.dump(output)
        self.kind.dump(output)
        self._dump_optional(self.value, output)

    @classmethod
    def load(cls, input):
        name = Symbol.load(input)
        kind = VariableKind.load(input)
        info = Variable(name, kind)
        info.value = cls._load_optional(Transformer, input)
        return info


class EnvironmentFrame:
    def __init__(self, initial_variables: list[Variable] = [],
                 *, pure_syntax_frame=False):
        assert all(isinstance(i, Variable) for i in initial_variables)
        self.variables = initial_variables

        # a pure syntax frame is one created for let-syntax/letrec-syntax and
        # has no equivalent run-time frame.
        self.pure_syntax_frame = pure_syntax_frame

    def add_variable(self, name: Symbol, kind: VariableKind):
        if self.get_variable(name) is not None:
            raise EnvironmentError(f'Duplicate variable: {name}', form=name)
        self.variables.append(Variable(name, kind))

    def add_macro(self, name: Symbol, transformer: Transformer):
        if self.get_variable(name) is not None:
            raise EnvironmentError(f'Duplicate variable: {name}', form=name)
        var = Variable(name, VariableKind.MACRO)
        self.variables.append(var)
        var.value = transformer

    def get_variable(self, name: Symbol) -> (None | Variable):
        for var in self.variables:
            if var.name == name:
                return var

        return None

    def get_variable_index(self, name: Symbol):
        i = 0
        for var in self.variables:
            if var.kind == VariableKind.NORMAL:
                if var.name == name:
                    return i
                i += 1

        return None

    def get_value(self, name: Symbol):
        for var in self.variables:
            if var.name == name:
                return var.value

        return None

    def set_value(self, name: Symbol, value):
        for var in self.variables:
            if var.name == name:
                var.value = value
                return True

        return False

    def __repr__(self):
        remark = ''
        if self.pure_syntax_frame:
            remark = ' [SYNTAX]'
        return f'<EnvironmentFrame{remark} vars={[str(v.name) for v in self.variables]}>'


class Environment:
    def __init__(self):
        self.parent = None

    def with_new_frame(self, variables):
        variables = [
            Variable(name, VariableKind.NORMAL)
            for name in variables
        ]
        return LocalEnvironment(
            parent=self,
            frame=EnvironmentFrame(variables),
        )

    def with_new_syntax_frame(self, bindings: dict[Symbol, Transformer]):
        variables = [
            Variable(name, VariableKind.MACRO, transformer)
            for name, transformer in bindings.items()
        ]
        return LocalEnvironment(
            parent=self,
            frame=EnvironmentFrame(variables, pure_syntax_frame=True),
        )

    def locate_local(self, name: Symbol, *, _frame_idx=0):
        raise NotImplementedError

    def add_import(self, import_set):
        raise NotImplementedError

    def lookup_symbol(self, sym: Symbol) -> SymbolInfo:
        raise NotImplementedError

    def get_all_names(self):
        raise NotImplementedError

    def add_define(self, sym: Symbol, kind: VariableKind):
        raise NotImplementedError

    def add_macro(self, name: Symbol, transformer: Transformer):
        raise NotImplementedError

    def add_read(self, sym: Symbol, source_file):
        raise NotImplementedError

    def check_for_undefined(self):
        raise NotImplementedError

    def print(self):
        env = self
        i = 0
        while env is not None:
            print(f'[{i}] {env}')
            env = env.parent
            i += 1

    def distance_to_parent(self, parent: 'Environment'):
        d = 0
        env = self
        while env != parent:
            if isinstance(env, LocalEnvironment) and \
               not env.frame.pure_syntax_frame:
                d += 1
            env = env.parent
            if env is None:
                raise ValueError('Not a parent environment')

        return d


class ToplevelEnvironment(Environment):
    def __init__(self):
        super().__init__()

        self.import_sets = []
        self.defined_symbols: dict[Symbol, Variable] = {}

        # for checking undefined symbols
        self.read_free_symbols = OrderedSet()

        # this is only used for collecting exports when processing a
        # define-library and is not a part of the environment itself.
        self.exports = []

        # used for resolving #$ symbols directly
        self._core = LibLoader().get_core()

    def locate_local(self, name: Symbol, *, _frame_idx=0):
        return None

    def add_import(self, import_set):
        self.import_sets.append(import_set)

    def lookup_symbol(self, sym: Symbol) -> SymbolInfo:
        if sym.name.startswith('#$'):
            info = self._core.lookup_external_name(sym)
            if info is not None:
                return info
            raise EnvironmentError(f'Invalid identifier: {sym}', form=sym)

        if sym.info is not None and sym.info.kind != SymbolKind.FREE:
            return sym.info

        if sym.info:
            # the symbol was FREE when it was defined, but maybe it's been
            # defined since then. we'll use the original symbol here for the
            # lookup.
            define_info = self.defined_symbols.get(sym.info.symbol)
        else:
            define_info = self.defined_symbols.get(sym)

        if define_info:
            kind = SymbolKind.DEFINED_NORMAL
            transformer = None
            if define_info.kind == VariableKind.UNHYGIENIC_MACRO:
                kind = SymbolKind.DEFINED_UNHYGIENIC_MACRO
            elif define_info.kind == VariableKind.MACRO:
                kind = SymbolKind.DEFINED_MACRO
                transformer = define_info.value
            info = SymbolInfo(
                symbol=sym,
                kind=kind,
                transformer=transformer,
            )
            return info

        for import_set in self.import_sets:
            result = import_set.lookup(sym)
            if result is not None:
                return result

        if sym.info:
            # if it was free at the time a macro attached info to sym, and it's
            # still free, we'll return the original sym.info, so the proper name
            # is used.
            return sym.info

        return SymbolInfo(
            symbol=sym,
            kind=SymbolKind.FREE,
        )

    def get_all_names(self):
        names = []
        for import_set in self.import_sets:
            names.extend(import_set.get_all_names())
        names.extend(self.defined_symbols.keys())
        return names

    def add_define(self, sym: Symbol, kind: VariableKind):
        self.defined_symbols[sym] = Variable(sym, kind)

    def add_macro(self, name: Symbol, transformer):
        self.defined_symbols[name] = Variable(
            name, VariableKind.MACRO,
            value=transformer)

    def add_read(self, sym: Symbol, source_file):
        assert not sym.name.startswith('##'), \
            'add_read is supposed to be called with external names ' \
            'but has been passesd a mangled name instead.'
        self.read_free_symbols.add((sym, source_file))

    def check_for_undefined(self):
        for sym, source_file in self.read_free_symbols:
            info = self.lookup_symbol(sym)
            if info.kind == SymbolKind.FREE:
                if sym.original:
                    sym = sym.original
                raise EnvironmentError(
                    f'Unbound variable is read: {sym}',
                    form=sym, source=source_file)

    def __repr__(self):
        return '<ToplevelEnvironment>'


class LocalEnvironment(Environment):
    def __init__(self,
                 parent: Environment,
                 frame: EnvironmentFrame):
        self.parent = parent
        self.frame = frame

        if isinstance(parent, ToplevelEnvironment):
            self.toplevel = parent
        else:
            self.toplevel = parent.toplevel

    def locate_local(self, name: Symbol, *, _frame_idx=0):
        var = self.frame.get_variable(name)
        if var is None:
            new_frame_idx = _frame_idx
            if not self.frame.pure_syntax_frame:
                new_frame_idx += 1
            return self.parent.locate_local(
                    name, _frame_idx=new_frame_idx)

        if var.kind == VariableKind.MACRO:
            return var.value
        elif var.kind == VariableKind.NORMAL:
            return [
                Integer(_frame_idx),
                Integer(self.frame.get_variable_index(name))
            ]
        else:
            assert False, f'unhandled case for {var.kind}'

    def add_import(self, import_set):
        return self.toplevel.add_import(import_set)

    def lookup_symbol(self, sym: Symbol) -> SymbolInfo:
        local = self.locate_local(sym)
        if local:
            if isinstance(local, Transformer):
                return SymbolInfo(
                    symbol=sym,
                    kind=SymbolKind.LOCAL_MACRO,
                    transformer=local,
                )
            else:
                return SymbolInfo(
                    symbol=sym,
                    kind=SymbolKind.LOCAL,
                    local_frame_idx=local[0],
                    local_var_idx=local[1],
                )

        if sym.info is not None:
            info = sym.info
            if info.kind == SymbolKind.LOCAL:
                info = copy(sym.info)
                info.local_frame_idx += self.distance_to_parent(sym.transform_env)
                return info

        return self.toplevel.lookup_symbol(sym)

    def get_all_names(self):
        names = []
        names.extend([v.name for v in self.frame.variables])
        names.extend(self.parent.get_all_names())
        return names

    def add_define(self, sym: Symbol, kind: VariableKind):
        return self.toplevel.add_define(sym, kind)

    def add_macro(self, name, transformer):
        self.frame.add_macro(name, transformer)

    def add_read(self, sym: Symbol, source_file):
        return self.toplevel.add_read(sym, source_file)

    def check_for_undefined(self):
        return self.toplevel.check_for_undefined()

    def __repr__(self):
        return f'<LocalEnvironment frame={self.frame}>'
