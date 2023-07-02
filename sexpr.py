#!/usr/bin/env python3

class ParseError(Exception):
    pass


class Symbol:
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name

    def __repr__(self):
        return f'<Symbol {self}>'


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
        value, i = _read(s, i)
        ls.append(value)

    return ls, i



def _read(s: str, i: int) -> tuple[int | Symbol | list, int]:
    if i >= len(s):
        raise ParseError('EOF while parsing input')

    i = _skip_whitespace(s, i)
    if i == len(s):
        raise ParseError('Nothing to read')

    if s[i] == '(':
        return _read_list(s, i)
    elif s[i] == ')':
        raise ParseError('Unbalanced parentheses')
    else:
        tok, i = _read_token(s, i)
        try:
            return int(tok), i
        except ValueError:
            return Symbol(tok), i


def read(s: str):
    value, _ = _read(s, 0)
    return value


def _print_sexpr(sexpr):
    if isinstance(sexpr, list):
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
