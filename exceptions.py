from machinetypes import Symbol
from snippet import show_snippet


class RunError(Exception):
    def __init__(self, msg, kind=None):
        assert kind is None or isinstance(kind, Symbol)
        self.msg = msg
        self.kind = kind

    def __repr__(self):
        return self.msg


class CompileError(Exception):
    def __init__(self, msg, form=None, source=None):
        self.msg = msg
        self.form = form
        self.source = source

    def __repr__(self):
        return self.msg

    def print_snippet(self):
        if self.form is None or self.source is None:
            return

        if self.form.src_start is None or self.form.src_end is None:
            return

        print()
        if self.source.filename:
            print(f'--- source file: {self.source.filename}')
        show_snippet(
            self.source.text, self.form.src_start, self.form.src_end,
            pre_lines=3, post_lines=3)


class AssembleError(Exception):
    pass
