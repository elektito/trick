from .libname import LibraryName
from .machinetypes import List


class Program:
    """This class is what the compiler's compile_program method returns. It
    contains the code in symbolic (unassembled) form, and also anything else
    that's needed to create the final output fasl.
    """

    def __init__(self, code: list,
                 defined_libs,
                 dependency_libs: set[LibraryName],
                 debug_info_enabled: bool):
        self.code = List.from_list_recursive(code)
        self.defined_libs = defined_libs
        self.dependency_libs = dependency_libs
        self.debug_info_enabled = debug_info_enabled
