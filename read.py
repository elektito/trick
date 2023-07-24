#!/usr/bin/env python3

from machinetypes import Char, Integer, List, Nil, Pair, String, Symbol, Bool


fold_case = False


class ParseError(Exception):
    pass


def is_separator(c):
    return c.isspace() or c in "()[]'`"


def perform_directive(directive):
    global fold_case
    if directive == '#!fold-case':
        fold_case = True
    elif directive == '#!no-fold-case':
        fold_case = False
    else:
        assert False


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

        directives = ['#!fold-case', '#!no-fold-case']
        for d in directives:
            if s[i:i+len(d)] == d and (i + len(d) == len(s) or is_separator(s[i+len(d)])):
                perform_directive(d)
                i += len(d)
                break

        if not s[i].isspace():
            return i
        i += 1

    return i


def _read_token(s, i):
    if s[i] == '|':
        string, i = _read_string(s, i, delim='|')
        tok = string.value
        return tok, i

    tok = ''
    while i < len(s) and not s[i].isspace() and not is_separator(s[i]):
        tok += s[i]
        i += 1
    return tok, i


def _read_list(s: str, i: int, end=')') -> tuple[List, int]:
    assert s[i] == ('(' if end == ')' else '[')

    i += 1
    items = []
    item_after_dot = None
    read_dot = False
    while i < len(s):
        i = _skip_whitespace(s, i)
        if i == len(s):
            raise ParseError('List not closed')
        if s[i] == end:
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


def _read_string(s: str, i: int, delim='"'):
    assert s[i] == delim

    i += 1
    read_str = ''
    escaped = False
    while i < len(s):
        if escaped:
            read_str += s[i]
            escaped = False
        elif s[i] == '\\':
            i += 1
            if i >= len(s):
                raise ParseError('String not closed')
            escape = s[i]
            map = {
                'a': '\a',
                'b': '\b',
                't': '\t',
                'n': '\n',
                'r': '\r',
                '"': '"',
                '\\': '\\',
                '|': '|',
            }
            if escape in map:
                read_str += map[escape]
            elif escape == ' ' or escape == '\n':
                newline_idx = s[i:].index('\n')
                i += newline_idx
                spaces_after = len(s[i+1:]) - len(s[i+1:].lstrip())
                i += spaces_after
            elif escape == 'x':
                try:
                    semicolon = s[i+1:].index(';')
                except ValueError:
                    raise ParseError('String hex escape not closed')
                hex_number = s[i+1:i+1+semicolon]
                try:
                    char_code = int(hex_number, 16)
                except ValueError:
                    raise ParseError(
                        f'Invalid string hex escape number: "{hex_number}"')
                try:
                    char = chr(char_code)
                except (ValueError, OverflowError):
                    raise ParseError(
                        f'Invalid character code in string hex escape: '
                        f'{hex(char_code)[2:]}')
                read_str += char
                i += semicolon + 1
            else:
                raise ParseError(f'Invalid escape character: "{escape}"')
        elif s[i] == delim:
            return String(read_str), i + 1
        else:
            read_str += s[i]
        i += 1

    raise ParseError('String not closed')


def _read_char(s: str, i: int) -> tuple[Char, int]:
    i += 2
    if i >= len(s):
        raise ParseError('EOF while reading character literal')

    desc = s[i]
    i += 1
    while i < len(s) and not s[i].isspace() and s[i] not in '()':
        desc += s[i]
        i += 1

    if desc in Char.name_to_code:
        return Char(Char.name_to_code[desc]), i
    else:
        if len(desc) == 1:
            char_code = ord(desc)
        elif desc.startswith('x'):
            try:
                char_code = int(desc[1:], 16)
            except ValueError:
                raise ParseError(f'Invalid character literal: #\\{desc}')
        else:
            raise ParseError(f'Invalid character literal: #\\{desc}')
        return Char(char_code), i


def _read(s: str, i: int = 0) -> tuple[None | Integer | Symbol | List | Bool | String | Char, int]:
    if i >= len(s):
        return None, len(s)

    i = _skip_whitespace(s, i)
    if i == len(s):
        return None, len(s)

    start = i

    if s[i] == '(':
        ret, i = _read_list(s, i, end=')')
    elif s[i] == '[':
        ret, i = _read_list(s, i, end=']')
    elif s[i] == ')':
        print(i, repr(s[i-3:i+3]))
        raise ParseError('Unbalanced parentheses')
    elif s[i] == '"':
        ret, i = _read_string(s, i)
    elif s[i] == "'":
        quoted, i = _read(s, i + 1)
        ret = List.from_list([Symbol('quote'), quoted])
    elif s[i] == '`':
        quoted, i = _read(s, i + 1)
        ret = List.from_list([Symbol('backquote'), quoted])
    elif s[i] == ',' and i < len(s) - 1 and s[i+1] == '@':
        unquoted, i = _read(s, i + 2)
        ret = List.from_list([Symbol('unquote-splicing'), unquoted])
    elif s[i] == ',':
        unquoted, i = _read(s, i + 1)
        ret = List.from_list([Symbol('unquote'), unquoted])
    elif i < len(s) - 1 and s[i:i+2] == '#f':
        ret, i = Bool(False), i + 2
    elif i < len(s) - 1 and s[i:i+2] == '#t':
        ret, i = Bool(True), i + 2
    elif i < len(s) - 1 and s[i:i+2] == '#x':
        tok, i = _read_token(s, i)
        tok = tok[2:]
        ret = Integer(tok, 16)
    elif i < len(s) - 1 and s[i:i+2] == '#\\':
        ret, i = _read_char(s, i)
    else:
        tok, i = _read_token(s, i)
        try:
            ret = Integer(tok)
        except ValueError:
            if fold_case:
                tok = tok.casefold()
            ret = Symbol(tok)

    ret.src_start = start
    ret.src_end = i
    return ret, i


def read(s: str, i: int = 0) -> tuple[None | Integer | Symbol | List | Bool | String | Char, int]:
    v, i = _read(s, i)
    if v == Symbol('.'):
        raise ParseError('Unexpected dot (.)')
    return v, i
