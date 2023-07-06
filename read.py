#!/usr/bin/env python3

from machinetypes import String, Symbol, Bool


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


def _read_list(s: str, i: int):
    assert s[i] == '('

    i += 1
    ls = []
    while i < len(s):
        i = _skip_whitespace(s, i)
        if i == len(s):
            raise ParseError('List not closed')
        if s[i] == ')':
            i += 1
            return ls, i
        value, i = read(s, i)
        ls.append(value)

    return ls, i


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


def read(s: str, i: int = 0) -> tuple[None | int | Symbol | list | Bool | String, int]:
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
        return [Symbol('quote'), quoted], i
    elif s == '#f':
        return Bool(False), i
    elif s == '#t':
        return Bool(True), i
    else:
        tok, i = _read_token(s, i)
        try:
            return int(tok), i
        except ValueError:
            return Symbol(tok), i


def _print_sexpr(sexpr):
    if sexpr == []:
            print('nil', end='')
    elif isinstance(sexpr, list):
        if len(sexpr) == 2 and sexpr[0] == Symbol('quote'):
            print("'", end='')
            _print_sexpr(sexpr[1])
        else:
            print('(', end='')
            for i, v in enumerate(sexpr):
                _print_sexpr(v)
                if i != len(sexpr) - 1:
                    print(' ', end='')
            print(')', end='')
    else:
        print(str(sexpr), end='')


def print_sexpr(sexpr):
    _print_sexpr(sexpr)
    print()
