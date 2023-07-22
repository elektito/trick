import os
from fasl import Fasl
from machinetypes import Symbol


def assoc(item, alist):
    for i in range(0, len(alist), 2):
        if alist[i] == item:
            return alist[i + 1]

    return None


def format_user_error(err):
    err_type = assoc(Symbol(':type'), err)
    msg = f'User error of type {err_type}'
    err_msg = assoc(Symbol(':msg'), err)
    if err_msg is not None:
        msg += f': {err_msg}'
    return msg


def compile_src_file_to_fasl(input_filename, output_filename, libs=[]):
    from compile import Compiler
    from assemble import Assembler

    lib_fasls = []
    for lib in libs:
        with open(lib, 'rb') as f:
            lib_fasls.append(Fasl.load(f))

    compiler = Compiler(lib_fasls)
    with open(input_filename) as f:
        text = f.read()

    fasl = Fasl()
    asm_code = compiler.compile_toplevel(text)
    fasl.defines = compiler.defined_symbols

    assembler = Assembler()
    assembler.assemble(asm_code, fasl)

    with open(output_filename, 'wb') as f:
        fasl.dump(f)


def compile_expr_to_fasl(expr, lib_fasls=[]) -> Fasl:
    from compile import Compiler
    from assemble import Assembler

    compiler = Compiler(lib_fasls)

    fasl = Fasl()
    asm_code = compiler.compile_form(expr, [], start_offset=0)
    fasl.defines = compiler.defined_symbols

    assembler = Assembler()
    assembler.assemble(asm_code, fasl)

    return fasl


def ensure_fasl(filename, libs=[]):
    name, ext = os.path.splitext(filename)
    fasl_name = name + '.fasl'
    if not os.path.exists(fasl_name):
        compile_src_file_to_fasl(filename, fasl_name, libs)
        return

    src_modify_time = os.path.getmtime(filename)
    fasl_modify_time = os.path.getmtime(fasl_name)
    if src_modify_time < fasl_modify_time:
        return

    compile_src_file_to_fasl(filename, fasl_name, libs)


def load_fasl_file(filename) -> Fasl:
    with open(filename, 'rb') as f:
        return Fasl.load(f)


def load_fasl_files(filenames) -> list[Fasl]:
    return [load_fasl_file(f) for f in filenames]
