from fasl import DefineInfo
from machinetypes import List, Symbol


class Program:
    """This class is what the compiler's compile_program method returns. It
    contains the code in symbolic (unassembled) form, and also anything else
    that's needed to create the final output fasl.
    """

    def __init__(self, code: list,
                 defines: dict[Symbol, DefineInfo],
                 debug_info_enabled: bool):
        self.code = List.from_list_recursive(code)
        self.defines = defines
        self.debug_info_enabled = debug_info_enabled
