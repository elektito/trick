import os
from os.path import isabs


def assoc(item, alist):
    for i in range(0, len(alist), 2):
        if alist[i] == item:
            return alist[i + 1]

    return None


def compile_src_file_to_fasl(input_filename, output_filename, libs=[], *,
                             include_paths=None, dbg_info=False):
    from compile import Compiler, Environment
    from assemble import Assembler
    from fasl import Fasl

    lib_fasls = []
    for lib in libs:
        with open(lib, 'rb') as f:
            lib_fasls.append(Fasl.load(f, lib))

    compiler = Compiler(lib_fasls, debug_info=dbg_info)
    if include_paths:
        compiler.include_paths = include_paths

    with open(input_filename) as f:
        text = f.read()

    fasl = Fasl()
    env = Environment()
    asm_code = compiler.compile_program(text, env, filename=input_filename)
    fasl.defines = env.defined_symbols

    assembler = Assembler()
    assembler.assemble(asm_code, fasl, dbg_info=dbg_info)

    with open(output_filename, 'wb') as f:
        fasl.dump(f)


def compile_expr_to_fasl(expr, lib_fasls=[]):
    from compile import Compiler, Environment
    from assemble import Assembler
    from fasl import Fasl

    compiler = Compiler(lib_fasls)

    fasl = Fasl()
    env = Environment()
    asm_code = compiler.compile_toplevel_form(expr, env)
    fasl.defines = env.defined_symbols

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


def load_fasl_file(filename):
    from fasl import Fasl
    with open(filename, 'rb') as f:
        return Fasl.load(f, filename)


def load_fasl_files(filenames):
    return [load_fasl_file(f) for f in filenames]


def find_shared(obj):
    from machinetypes import Pair, Vector, Reference, Symbol, shareable_types

    seen = {}
    shared = set()
    stack = [obj]

    # we used to do this recursively, but then we'd run into python's maximum
    # recursion depth for large-ish lists.
    while stack:
        obj = stack.pop()
        if not isinstance(obj, shareable_types):
            continue
        if obj in seen:
            shared.add(obj)
            continue
        seen[obj] = Reference(Symbol.gensym())

        if isinstance(obj, Pair):
            stack.append(obj.car)
            stack.append(obj.cdr)
        elif isinstance(obj, Vector):
            for e in obj:
                stack.append(e)
        else:
            assert False

    return shared
