from machinetypes import Char, String, shareable_types, Nil, Pair, Symbol, Vector
from utils import find_shared
from enum import Enum


class PrintMode(Enum):
    # ignore any cycles or shared structures; if there are cycles in the input,
    # we might crash.
    Simple = 1

    # use datum labels only when there are cycles in the input
    Cyclic = 2

    # use datum labels for all shared structures
    Shared = 3


class PrintStyle(Enum):
    # machine-readable output like the "write" procedure should
    Write = 1

    # human-readable output like the "display" procedure should
    Display = 2


class Printer:
    def __init__(self, obj, style=PrintStyle.Write, mode=PrintMode.Cyclic):
        self._obj = obj
        self._style = style

        if isinstance(obj, shareable_types):
            if mode == PrintMode.Simple:
                self._shared = {}
            elif mode == PrintMode.Cyclic:
                self._shared = find_cycles(obj)
            elif mode == PrintMode.Shared:
                self._shared = find_shared(obj)
            else:
                raise ValueError(f'Invalid print mode: {mode}')
        else:
            self._shared = {}

        self._reset()

    def print(self):
        self._reset()
        return self._print(self._obj)

    def _reset(self):
        self._labels = {}
        self._label_count = 0

    def _print(self, obj):
        s = ''
        if obj in self._shared:
            n = self._labels.get(obj)
            if n is not None:
                return f'#{n}#'
            n = self._assign_label(obj)
            s = f'#{n}='

        if isinstance(obj, Pair):
            s += self._print_pair(obj)
        elif isinstance(obj, Vector):
           s += self._print_vector(obj)
        else:
            s += self._print_atom(obj)

        return s

    def _print_pair(self, pair):
        special = {
            Symbol('quote'): "'",
            Symbol('quasiquote'): '`',
            Symbol('unquote'): ',',
            Symbol('unquote-splicing'): ',@',
        }
        if (pair.car in special or (isinstance(pair.car, Symbol) and pair.car.original in special)) and \
           isinstance(pair.cdr, Pair) and \
           isinstance(pair.cdr.cdr, Nil):
            if isinstance(pair.car, Symbol) and pair.car.original in special:
                s = special[pair.car.original]
            else:
                s = special[pair.car]
            if pair.cdr.car in self._shared:
                n = self._labels.get(pair.cdr.car)
                if n is None:
                    s += self._print(pair.cdr.car)
                else:
                    s += f'#{n}#'
            else:
                s += self._print(pair.cdr.car)
            return s

        s = '('
        while isinstance(pair, Pair):
            if s != '(':
                s += ' '
            s += self._print(pair.car)

            if pair.cdr in self._shared:
                s += f' . '
                s += self._print(pair.cdr)
                pair = Nil()
                break

            pair = pair.cdr

        if not isinstance(pair, Nil):
            s += ' . '
            s += self._print(pair)

        s += ')'
        return s

    def _print_vector(self, vec):
        s = '#('
        for e in vec:
            if s != '#(':
                s += ' '
            s += self._print(e)

        s += ')'

        return s

    def _print_atom(self, obj):
        if self._style == PrintStyle.Write:
            return str(obj)
        else:
            return self._display_atom(obj)

    def _display_atom(self, obj):
        if isinstance(obj, String):
            return obj.value
        elif isinstance(obj, Symbol):
            return obj.name
        elif isinstance(obj, Char):
            return chr(obj.char_code)
        else:
            return str(obj)

    def _assign_label(self, obj):
        assert isinstance(obj, (Pair, Vector))
        n = self._label_count
        self._label_count += 1
        self._labels[obj] = n
        return n


def find_cycles(obj):
    parents = set()
    cycles = set()
    stack = [(obj, parents)]

    # we used to do this recursively, but then we'd run into python's maximum
    # recursion depth for large-ish lists.
    while stack:
        obj, parents = stack.pop()
        if not isinstance(obj, (Pair, Vector)):
            continue
        parents = set(parents | {obj})
        if isinstance(obj, Pair):
            children = [obj.car, obj.cdr]
        elif isinstance(obj, Vector):
            children = [ch for ch in obj]
        else:
            children = []

        for child in children:
            if child in parents:
                cycles.add(child)
            else:
                stack.append((child, parents))

    return cycles
