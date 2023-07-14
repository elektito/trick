#!/usr/bin/env python3

from machinetypes import List, Nil, Pair, String, Symbol, Bool


class ParseError(Exception):
    pass


def _skip_whitespace(s: str, i: int) -> int:
    while i < len(s):
        if s[i] == ';':
            i += 1
            while i < len(s) and s[i] != '\n':
                i += 1
            if i == len(s):
                return i
        if s[i:i+2] == '#|':
            i += 2
            while i < len(s) and s[i:i+2] != '|#':
                i += 1
            i += 2
            if i >= len(s):
                return len(s)
        if not s[i].isspace():
            return i
        i += 1

    return i


def _read_token(s, i):
    tok = ''
    while i < len(s) and not s[i].isspace() and s[i] not in '()':
        tok += s[i]
        i += 1
    return tok, i


def _read_list(s: str, i: int) -> tuple[List, int]:
    assert s[i] == '('

    i += 1
    items = []
    item_after_dot = None
    read_dot = False
    while i < len(s):
        i = _skip_whitespace(s, i)
        if i == len(s):
            raise ParseError('List not closed')
        if s[i] == ')':
            i += 1
            ls = List.from_list(items)
            if item_after_dot is not None:
                ls.last().cdr = item_after_dot
            elif read_dot:
                raise ParseError('Expected an item after dot (.)')
            return ls, i

        value, i = _read(s, i)
        if value == Symbol('.'):
            if items == []:
                raise ParseError('No item before dot')
            elif not read_dot:
                read_dot = True
            else:
                raise ParseError('More than one dot in list')
        elif read_dot:
            if item_after_dot is not None:
                raise ParseError('More than one item after dot')
            item_after_dot = value
        else:
            items.append(value)

    raise ParseError('List not closed')


def _read_string(s: str, i: int):
    assert s[i] == '"'

    i += 1
    read_str = ''
    escaped = False
    while i < len(s):
        if escaped:
            read_str += s[i]
            escaped = False
        elif s[i] == '\\':
            escaped = True
        elif s[i] == '"':
            return String(read_str), i + 1
        else:
            read_str += s[i]
        i += 1

    raise ParseError('String not closed')


def _read(s: str, i: int = 0) -> tuple[None | int | Symbol | List | Bool | String, int]:
    if i >= len(s):
        return None, len(s)

    i = _skip_whitespace(s, i)
    if i == len(s):
        return None, len(s)

    if s[i] == '(':
        return _read_list(s, i)
    elif s[i] == ')':
        raise ParseError('Unbalanced parentheses')
    elif s[i] == '"':
        return _read_string(s, i)
    elif s[i] == "'":
        quoted, i = read(s, i + 1)
        return List.from_list([Symbol('quote'), quoted]), i
    elif s[i] == '`':
        quoted, i = read(s, i + 1)
        return List.from_list([Symbol('backquote'), quoted]), i
    elif s[i] == ',' and i < len(s) - 1 and s[i+1] == '@':
        unquoted, i = read(s, i + 2)
        return List.from_list([Symbol('unquote-splicing'), unquoted]), i
    elif s[i] == ',':
        unquoted, i = read(s, i + 1)
        return List.from_list([Symbol('unquote'), unquoted]), i
    elif i < len(s) - 1 and s[i:i+2] == '#f':
        return Bool(False), i + 2
    elif i < len(s) - 1 and s[i:i+2] == '#t':
        return Bool(True), i + 2
    elif i < len(s) - 1 and s[i:i+2] == '#x':
        tok, i = _read_token(s, i)
        tok = tok[2:]
        return int(tok, 16), i
    else:
        tok, i = _read_token(s, i)
        try:
            return int(tok), i
        except ValueError:
            return Symbol(tok), i


def read(s: str, i: int = 0) -> tuple[None | int | Symbol | List | Bool | String, int]:
    v, i = _read(s, i)
    if v == Symbol('.'):
        raise ParseError('Unexpected dot (.)')
    return v, i
