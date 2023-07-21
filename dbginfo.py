class DebugInfoRecord:
    def __init__(self, src_start, src_end, code_start, code_end):
        self.src_start = src_start
        self.src_end = src_end
        self.code_start = code_start
        self.code_end = code_end
