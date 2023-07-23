import sys
import subprocess


def is_stdout_tty():
    return sys.stdout.isatty()


def is_stderr_tty():
    return sys.stderr.isatty()


class ColorCodes(object):
    def __init__(self):
        self.red = ''
        self.green = ''
        self.orange = ''
        self.blue = ''
        self.bold = ''
        self.reset = ''

        if not is_stdout_tty() or not is_stderr_tty():
            return

        try:
            self.red = subprocess.check_output(['tput', 'setaf', '1']).decode()
            self.green = subprocess.check_output(['tput', 'setaf', '2']).decode()
            self.orange = subprocess.check_output(['tput', 'setaf', '3']).decode()
            self.blue = subprocess.check_output(['tput', 'setaf', '3']).decode()
            self.bold = subprocess.check_output(['tput', 'bold']).decode()
            self.reset = subprocess.check_output(['tput', 'sgr0']).decode()
        except subprocess.CalledProcessError:
            pass


cc = ColorCodes()


def split_lines(text):
    i = 0
    lines = []
    line = ''
    line_start = 0
    while i < len(text):
        if text[i] == '\n':
            lines.append((line, line_start, i))
            line_start = i + 1
            line = ''
        else:
            line += text[i]
        i += 1
    if line:
        lines.append((line, line_start, len(text)))
    return lines


def index_to_line_and_col(text: str, index: int) -> tuple[int, int]:
    "convert an index in the given text to a line/column number pair."

    line_no = text[:index].count('\n') + 1

    try:
        col_no = index - text[:index].rindex('\n')
    except ValueError:
        col_no = index + 1

    return line_no, col_no


def show_snippet_dumb(text, start, end, *, pre_lines=0, post_lines=0):
    lines = split_lines(text)

    first_line, _ = index_to_line_and_col(text, start)
    last_line, _ = index_to_line_and_col(text, end)

    max_line_number_size = len(str(last_line))

    indent = 0

    for line_no, (line, line_start, line_end) in enumerate(lines, 1):
        prefix = f'|{line_no: >{max_line_number_size}}| '
        if line_start <= start < line_end and line_start <= end < line_end:
            # only line
            s = start - line_start
            e = end - line_start
            indent = s
            line = line[s:e]
        elif line_start <= start < line_end:
            # first line
            s = start - line_start
            indent = s
            line = line[s:]
        elif line_start <= end <= line_end:
            # last line
            e = end - line_start
            if line[:indent].isspace() or indent == 0:
                line = line[indent:e]
            else:
                line = line[:e].lstrip()
        elif line_start > start and line_end <= end:
            # middle line
            if line[:indent].isspace() or indent == 0:
                line = line[indent:]
            else:
                line = line_end.lstrip()
        elif line_no >= first_line - pre_lines and line_no <= last_line + post_lines:
            # pre/post lines; not supported in dumb version
            continue
        else:
            # not part of the snippet
            continue

        print(f'{prefix}{line}')


def show_snippet_terminal(text, start, end, *,
                          pre_lines=0,
                          post_lines=0):
    lines = split_lines(text)

    first_line, _ = index_to_line_and_col(text, start)
    last_line, _ = index_to_line_and_col(text, end)

    mark_start = cc.orange
    mark_end = cc.reset
    max_line_number_size = len(str(last_line))

    for line_no, (line, line_start, line_end) in enumerate(lines, 1):
        prefix = f'|{line_no: >{max_line_number_size}}| '
        if line_start <= start < line_end and line_start <= end < line_end:
            # only line
            s = start - line_start
            e = end - line_start
            before = line[:s]
            middle = line[s:e]
            after = line[e:]
            marked_line = before + mark_start + middle + mark_end + after
        elif line_start <= start < line_end:
            # first line
            s = start - line_start
            before = line[:s]
            rest = line[s:]
            marked_line = before + mark_start + rest + mark_end
        elif line_start <= end <= line_end:
            # last line
            e = end - line_start
            before = line[:e]
            rest = line[e:]
            marked_line = mark_start + before + mark_end + rest
        elif line_start > start and line_end <= end:
            # middle line
            marked_line = mark_start + line + mark_end
        elif line_no >= first_line - pre_lines and line_no <= last_line + post_lines:
            # pre/post lines
            marked_line = line
        else:
            # not part of the snippet
            continue

        print(f'{prefix}{marked_line}')

def show_snippet(text, start, end, *, pre_lines=0, post_lines=0):
    if is_stdout_tty():
        show_snippet_terminal(
            text, start, end,
            pre_lines=pre_lines,
            post_lines=post_lines)
    else:
        show_snippet_dumb(
            text, start, end,
            pre_lines=pre_lines,
            post_lines=post_lines)
