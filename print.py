from machinetypes import Nil, Pair


class SharedPrinter:
    def __init__(self, obj):
        if isinstance(obj, Pair):
            self._obj = obj
            self._shared = self._obj.find_shared()
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

        if not isinstance(obj, Pair):
            s += str(obj)
        else:
            s += self._print_pair(obj)

        return s

    def _print_pair(self, pair, *, at_start=True):
        s = ''

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
                    s += ' . '
                    s += self._print(pair.cdr, at_start=True)
                else:
                    s += f' . #{n}#)'
            else:
                s += self._print_pair(pair.cdr, at_start=False)
        else:
            s += ' . '
            s += self._print(pair.cdr)
            s += ')'

        return s

    def _assign_label(self, obj):
        assert isinstance(obj, Pair)
        n = self._label_count
        self._label_count += 1
        self._labels[obj] = n
        return n
