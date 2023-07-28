from machinetypes import shareable_types, Nil, Pair, Symbol, Vector
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


class Printer:
    def __init__(self, obj, mode=PrintMode.Cyclic):
        if isinstance(obj, shareable_types):
            self._obj = obj
            if mode == PrintMode.Simple:
                self._shared = {}
            elif mode == PrintMode.Cyclic:
                self._shared = find_cycles(obj)
            elif mode == PrintMode.Shared:
                self._shared = find_shared(obj)
            else:
                raise ValueError(f'Invalid print mode: {mode}')
        else:
            self._obj = obj
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
        if obj in self._shared and obj not in self._labels:
            n = self._assign_label(obj)
            s = f'#{n}='

        if isinstance(obj, Pair):
            s += self._print_pair(obj)
        elif isinstance(obj, Vector):
           s += self._print_vector(obj)
        else:
            s += str(obj)

        return s

    def _print_pair(self, pair, *, at_start=True):
        s = ''

        special = {
            Symbol('quote'): "'",
            Symbol('quasiquote'): '`',
            Symbol('unquote'): ',',
            Symbol('unquote-splicing'): ',@',
        }
        if at_start and \
           pair.car in special and \
           isinstance(pair.cdr, Pair) and \
           isinstance(pair.cdr.cdr, Nil):
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

        if at_start:
            s += '('
        else:
            s += ' '

        if pair.car in self._shared:
            n = self._labels.get(pair.car)
            if n is None:
                n = self._assign_label(pair.car)
                s += f'#{n}='
                s += self._print(pair.car)
            else:
                s += f'#{n}#'
        else:
            s += self._print(pair.car)

        if isinstance(pair.cdr, Nil):
            s += ')'
        elif isinstance(pair.cdr, Pair):
            if pair.cdr in self._shared:
                n = self._labels.get(pair.cdr)
                if n is None:
                    n = self._assign_label(pair.cdr)
                    s += f' . #{n}='
                    s += self._print_pair(pair.cdr, at_start=True)
                    s += ')'
                else:
                    s += f' . #{n}#)'
            else:
                s += self._print_pair(pair.cdr, at_start=False)
        else:
            s += ' . '
            s += self._print(pair.cdr)
            s += ')'

        return s

    def _print_vector(self, vec):
        s = '#('
        at_start = True
        for e in vec:
            if at_start:
                at_start = False
            else:
                s += ' '
            if e in self._shared:
                n = self._labels.get(e)
                if n is None:
                    n = self._assign_label(e)
                    s += f'#{n}='
                    s += self._print(e)
                else:
                    s += f'#{n}#'
            else:
                s += self._print(e)

        s += ')'

        return s

    def _assign_label(self, obj):
        assert isinstance(obj, (Pair, Vector))
        n = self._label_count
        self._label_count += 1
        self._labels[obj] = n
        return n


def find_cycles(obj, parents=None, cycles=None):
    if not isinstance(obj, (Pair, Vector)):
        return cycles
    if parents is None:
        parents = set()
    if cycles is None:
        cycles = set()

    # `parents |= {obj}` would mutate parents, so we use this to get a fresh
    # copy
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
            find_cycles(child, parents, cycles)

    return cycles
