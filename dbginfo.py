class DebugInfoRecord:
    def __init__(self, src_start, src_end, code_start, code_end, form):
        self.src_start = src_start
        self.src_end = src_end
        self.code_start = code_start
        self.code_end = code_end
        self.form = form

    def __repr__(self):
        return f'<Dbg src={self.src_start}-{self.src_end} asm={self.code_start}-{self.code_end} form={self.form}>'
