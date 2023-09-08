from libname import LibraryName
from machinetypes import List, Nil, Pair, String, Symbol, Vector
from symbolinfo import SymbolInfo, SymbolKind


libtrick = LibraryName.create('trick')
trick_append_info = SymbolInfo(
    symbol=Symbol('##1-5trick-append'),
    kind=SymbolKind.DEFINED_NORMAL,
    library_name=libtrick)
trick_list_info = SymbolInfo(
    symbol=Symbol('##1-5trick-list'),
    kind=SymbolKind.DEFINED_NORMAL,
    library_name=libtrick)
trick_list_star_info = SymbolInfo(
    symbol=Symbol('##1-5trick-list*'),
    kind=SymbolKind.DEFINED_NORMAL,
    library_name=libtrick)
trick_list_to_vector_info = SymbolInfo(
    symbol=Symbol('##1-5trick-list->vector'),
    kind=SymbolKind.DEFINED_NORMAL,
    library_name=libtrick)


QQ_APPEND = Symbol.gensym(
    String('append'),
    no_shortname_prefix=True,
    info=trick_append_info)
QQ_LIST = Symbol.gensym(
    String('list'),
    no_shortname_prefix=True,
    info=trick_list_info)
QQ_LIST_STAR = Symbol.gensym(
    String('list*'),
    no_shortname_prefix=True,
    info=trick_list_star_info)
QQ_LIST_TO_VECTOR = Symbol.gensym(
    String('list->vector'),
    no_shortname_prefix=True,
    info=trick_list_to_vector_info)
QQ_QUOTE = Symbol.gensym(String('quote'))


class QuasiquoteError(Exception):
    def __init__(self, msg, form=None, source=None):
        self.msg = msg
        self.form = form
        self.source = source

    def __repr__(self):
        return self.msg


class Quasiquote:
    def __init__(self, *, simplify=True):
        self._simplify_enabled = simplify

    def process(self, form):
        assert isinstance(form, Pair)
        assert len(form) == 2
        assert form.car == Symbol('quasiquote')
        form = form.cdr.car

        result = self._process(form, 1)
        if self._simplify_enabled:
            result = self._simplify(result)
        result = self._remove_tokens(result)
        return result

    def _process(self, form, level):
        if isinstance(form, Vector):
            return self._list(
                QQ_LIST_TO_VECTOR,
                self._process_list(form.to_trick_list(), level))
        elif self._is_atom(form):
            if level == 1:
                return self._list(QQ_QUOTE, form)
            else:
                return form
        elif self._is_unquote(form):
            if level == 1:
                return cadr(form)
            else:
                return self._process_list(form, level - 1)
        elif self._is_unquote_splicing(form):
            raise QuasiquoteError(
                'unquote-splicing immediately inside quasiquote',
                form=form)
        elif self._is_quasiquote(form):
            return self._process_list(form, level + 1)
        else:
            return self._process_list(form, level)

    def _process_list(self, form: List, level):
        rest, tail = self._split_improper_tail(form)
        append_form = Pair(
            QQ_APPEND,
            List.from_list([
                self._process_list_item(i, level) for i in rest]))
        if tail == Nil():
            return append_form
        else:
            return self._append(
                append_form,
                Pair(self._process_list_tail(tail, level),
                     Nil()))

    def _process_list_item(self, form, level):
        if isinstance(form, Vector):
            return self._list(
                QQ_LIST,
                self._list(QQ_LIST_TO_VECTOR,
                           self._process_list(form.to_trick_list(), level)))
        elif self._is_atom(form):
            return self._list(QQ_QUOTE, self._list(form))
        elif self._is_unquote(form):
            if level == 1:
                return self._list(QQ_LIST, cadr(form))
            else:
                return self._list(QQ_LIST, self._process_list(form, level - 1))
        elif self._is_unquote_splicing(form):
            if level == 1:
                return cadr(form)
            else:
                return self._list(
                    QQ_LIST,
                    self._process_list(form, level - 1))
        elif self._is_quasiquote(form):
            return self._list(QQ_LIST, self._process_list(form, level + 1))
        else:
            return self._list(QQ_LIST, self._process_list(form, level))

    def _process_list_tail(self, form, level):
        if isinstance(form, Vector):
            return self._list(
                QQ_LIST_TO_VECTOR,
                self._process_list(form.to_trick_list(), level))
        elif self._is_atom(form):
            return self._list(QQ_QUOTE, form)
        elif self._is_unquote(form):
            if level == 1:
                return cadr(form)
            else:
                return self._process_list(form, level - 1)
        elif self._is_unquote_splicing(form):
            raise QuasiquoteError(
                'unquote-splicing in dotted tail', form=form)
        elif self._is_quasiquote(form):
            return self._process_list(form, level + 1)
        else:
            return self._process_list(form, level)

    def _split_improper_tail(self, ls):
        # () => () -- ()
        # (a) => (a) -- ()
        # (a b) => (a b) -- ()
        # (a . b) => (a) -- b
        # (a . ,b) => (a) -- ,b
        # (a b . ,c) => (a b) -- ,c
        # (a b . c) => (a b) -- c
        # (a . `c) => a -- `c
        # (a b . `c) => (a b) -- `c
        if ls == Nil():
            return Nil(), Nil()
        elif self._is_atom(cdr(ls)):
            return self._list(car(ls)), cdr(ls)
        elif self._is_unquote(cdr(ls)):
            return self._list(car(ls)), cdr(ls)
        elif self._is_unquote_splicing(cdr(ls)):
            return self._list(car(ls)), cdr(ls)
        elif self._is_quasiquote(cdr(ls)):
            return self._list(car(ls)), cdr(ls)
        else:
            r, t = self._split_improper_tail(cdr(ls))
            return Pair(car(ls), r), t

    def _is_atom(self, form):
        return not isinstance(form, Pair)

    def _is_unquote(self, form):
        # FIXME incorrect; what if "unquote" is renamed?
        return isinstance(form, Pair) and car(form) == Symbol('unquote')

    def _is_unquote_splicing(self, form):
        # FIXME incorrect; what if "unquote-splicing" is renamed?
        return isinstance(form, Pair) and car(form) == Symbol('unquote-splicing')

    def _is_quasiquote(self, form):
        # FIXME incorrect; what if "unquote-splicing" is renamed?
        return isinstance(form, Pair) and car(form) == Symbol('quasiquote')

    def _is_quote_nil(self, form):
        # '()
        if not isinstance(form, Pair):
            return False
        if car(form) != QQ_QUOTE:
            return False
        if not isinstance(form.cdr, Pair):
            return False
        if cddr(form) != Nil():
            return False
        return cadr(form) == Nil()

    def _is_null_or_quoted(self, x):
        return x == Nil() or \
            (isinstance(x, Pair) and x.car == QQ_QUOTE)

    def _is_splicing_frob(self, x):
        # FIXME what if "unquote-splicing" is renamed?
        return isinstance(x, Pair) and x.car == Symbol('unquote-splicing')

    def _is_frob(self, x):
        # FIXME what if "unquotee" and/or "unquote-splicing" is renamed?
        return isinstance(x, Pair) and x.car in [Symbol('unquote'), Symbol('unquote-splicing'), Symbol('quasiquote')]

    def _reversed(self, ls: (Pair | Nil), acc: List = Nil()):
        if isinstance(ls, Nil):
            return acc

        if isinstance(ls.cdr, Pair):
            return self._reversed(ls.cdr, Pair(ls.car, acc))
        else:
            return Pair(ls.car, acc)

    def _append(self, ls1, ls2):
        assert isinstance(ls1, List)

        if ls1 == Nil():
            return ls2

        if ls2 == Nil():
            return ls1

        return Pair(ls1.car, self._append(ls1.cdr, ls2))

    def _list(self, *items):
        return List.from_list(list(items))

    def _list_star(self, *items):
        assert items != []
        if len(items) == 1:
            return items[0]
        return Pair(items[0], self._list_star(items[1:]))

    def _maptree(self, fn, x):
        if self._is_atom(x):
            return fn(x)

        a = fn(car(x))
        d = self._maptree(fn, cdr(x))

        if a == car(x) and d == cdr(x):  # eqv?
            return x
        else:
            return Pair(a, d)

    def _simplify(self, form):
        if self._is_atom(form):
            return form

        if form.car != QQ_QUOTE:
            form = self._maptree(self._simplify, form)

        if form.car == QQ_APPEND:
            return self._simplify_args(form)
        else:
            return form

    def _simplify_args(self, form):
        args = self._reversed(cdr(form))
        result = Nil()
        while args != Nil():
            if self._is_atom(car(args)):
                result = self._attach_append(
                    QQ_APPEND, car(args), result)
            elif caar(args) == QQ_LIST and \
                 not any(self._is_splicing_frob(i)
                         for i in cdar(args)):
                result = self._attach_conses(cdar(args), result)
            elif caar(args) == QQ_LIST_STAR and \
                 not any(self._is_splicing_frob(i)
                         for i in cdar(args)):
                result = self._attach_conses(
                    self._reversed(
                        cdr(self._reversed(cdar(args)))),
                    self._attach_append(
                        QQ_APPEND,
                        car(last(car(args))),
                        result))
            elif caar(args) == QQ_QUOTE and \
                 isinstance(cadar(args), Pair) and \
                 not self._is_frob(cadar(args)) and \
                 isinstance(cddar(args), Nil):
                result = self._attach_conses(
                    self._list(
                        self._list(QQ_QUOTE,
                                   caadar(args))),
                    result)
            else:
                result = self._attach_append(
                    QQ_APPEND, car(args), result)

            args = cdr(args)

        return result

    def _attach_conses(self, items, result):
        if all(self._is_null_or_quoted(i) for i in items) and \
           self._is_null_or_quoted(result):
            return self._list(
                QQ_QUOTE,
                self._append(
                    List.from_list([cadr(i) for i in items]),
                    cadr(result)))
        elif result == Nil() or self._is_quote_nil(result):
            return Pair(QQ_LIST, items)
        elif isinstance(result, Pair) and result.car in [QQ_LIST, QQ_LIST_STAR]:
            return Pair(car(result),
                        self._append(items, cdr(result)))
        else:
            return Pair(QQ_LIST_STAR,
                        self._append(items, self._list(result)))

    def _attach_append(self, op, item, result):
        if self._is_null_or_quoted(item) and self._is_null_or_quoted(result):
            item_cadr = cadr(item)
            result_cadr = cadr(result)

            if item_cadr == Nil() and result_cadr == Nil():
                return self._list(QQ_QUOTE, Nil())
            elif item_cadr == Nil():
                return self._list(QQ_QUOTE, result_cadr)
            elif result == Nil():
                return self._list(QQ_QUOTE, item_cadr)
            else:
                if not isinstance(item_cadr, List):
                    item_cadr = [item_cadr]
                if not isinstance(result_cadr, List):
                    result_cadr = [result_cadr]
                return self._list(
                    QQ_QUOTE,
                    self._append(item_cadr, result_cadr))
        elif result == Nil() or self._is_quote_nil(result):
            if self._is_splicing_frob(result):
                return self._list(op, item)
            else:
                return item
        elif isinstance(result, Pair) and car(result) == op:
            return self._list_star(car(result), item, cdr(result))
        else:
            return self._list(op, item, result)

    def _remove_tokens(self, x):
        # in the old define-macro version, this function replaced gensyms like
        # qq-list, qq-append, etc. with their actual names like list, append,
        # etc. We won't be doing that here anymore, since the gensyms know have
        # enough lexical information associated with them to make them aliases
        # of the originals. We actually do not want to use the original names so
        # we won't capture any variables with those names.
        #
        # so this function's only job right now is to replace certain list*
        # invocations with simpler cons calls. I was just too lazy to properly
        # re-write and rename it (though not so bored that I wouldn't want to
        # write a long comment explaining things).

        if x == QQ_LIST:
            return x
        elif x == QQ_APPEND:
            return x
        elif x == QQ_LIST_STAR:
            return x
        elif x == QQ_QUOTE:
            return Symbol('#$quote')
        elif x == QQ_LIST_TO_VECTOR:
            return x
        elif self._is_atom(x):
            return x
        elif car(x) == QQ_LIST_STAR and \
             isinstance(cddr(x), Pair) and \
             cdddr(x) == Nil():
            return Pair(Symbol('#$cons'),
                        self._maptree(self._remove_tokens, x.cdr))
        else:
            return self._maptree(self._remove_tokens, x)


# since this implementation is based on a cl backquote implementation, we'll be
# used c*r functions with behavior similar to cl (namely car/cdr of the empty
# list being the empty list).

def car(x: List):
    if x == Nil():
        return Nil()
    else:
        return x.car


def cdr(x: List):
    if x == Nil():
        return Nil()
    else:
        return x.cdr


def caar(x: List):
    return car(car(x))


def cadr(x: List):
    return car(cdr(x))


def cdar(x: List):
    return cdr(car(x))


def cddr(x: List):
    return cdr(cdr(x))


def cadar(x: List):
    return car(cdr(car(x)))


def cddar(x: List):
    return cdr(cdr(car(x)))


def cdddr(x: List):
    return cdr(cdr(cdr(x)))


def caadar(x: List):
    return car(car(cdr(car(x))))



def last(x: List):
    if x == Nil():
        return Nil()
    assert isinstance(x, Pair)
    if isinstance(x.cdr, Pair):
        return last(x.cdr)
    else:
        return x
