import glob
import os


STR_ENCODING = 'utf-8'


class OrderedSet:
    def __init__(self, values=None):
        # since python 3.7+ dictionaries retain order, we'll be using the
        # dictionary keys as our set.
        self.set = {}
        if values is not None:
            for v in values:
                self.set[v] = True

    def add(self, value):
        self.set[value] = True

    def __contains__(self, value):
        return value in self.set

    def __str__(self):
        return f'{{{", ".join(str(i) for i in self.set)}}}'

    def __repr__(self):
        return f'OrderedSet({str(self)})'

    def __iter__(self):
        return iter(self.set.keys())


def assoc(item, alist):
    for i in range(0, len(alist), 2):
        if alist[i] == item:
            return alist[i + 1]

    return None


def compile_src_file_to_fasl(input_filename, output_filename, libs=[], *,
                             include_paths=None, dbg_info=False):
    from .compile import Compiler
    from .assemble import Assembler
    from .fasl import Fasl

    lib_fasls = []
    for lib in libs:
        with open(lib, 'rb') as f:
            lib_fasls.append(Fasl.load(f, lib))

    compiler = Compiler(lib_fasls, debug_info=dbg_info)
    if include_paths:
        compiler.include_paths = include_paths

    with open(input_filename) as f:
        text = f.read()

    program = compiler.compile_program(text, filename=input_filename)
    assembler = Assembler()
    fasl = assembler.assemble_program(program)

    with open(output_filename, 'wb') as f:
        fasl.dump(f)


def compile_expr_to_fasl(expr, lib_fasls=None, env=None):
    from .compile import Compiler, ToplevelEnvironment
    from .assemble import Assembler
    from .fasl import Fasl

    lib_fasls = lib_fasls or []
    env = env or ToplevelEnvironment()

    compiler = Compiler(lib_fasls)

    fasl = Fasl()
    asm_code = compiler.compile_toplevel_form(expr, env)

    assembler = Assembler()
    assembler.assemble(asm_code, fasl)

    return fasl


def ensure_fasl(main_src_filename,
                all_src_filenames,
                fasl_filename=None,
                libs=[]):
    if fasl_filename is None:
        name, ext = os.path.splitext(main_src_filename)
        fasl_filename = name + '.fasl'

    if not os.path.exists(fasl_filename):
        compile_src_file_to_fasl(main_src_filename, fasl_filename, libs)
        return

    src_modify_times = [os.path.getmtime(f) for f in all_src_filenames]
    fasl_modify_time = os.path.getmtime(fasl_filename)
    if all(t < fasl_modify_time for t in src_modify_times):
        return

    compile_src_file_to_fasl(main_src_filename, fasl_filename, libs)


def ensure_stdlib(fasl_filename: str):
    stdlib_main_src_filename = 'trick/scm/stdlib/defs.scm'
    stdlib_all_src_filenames = glob.glob('stdlib/*.scm')
    ensure_fasl(stdlib_main_src_filename,
                stdlib_all_src_filenames,
                fasl_filename)


def load_fasl_file(filename):
    from .fasl import Fasl
    with open(filename, 'rb') as f:
        return Fasl.load(f, filename)


def load_fasl_files(filenames):
    return [load_fasl_file(f) for f in filenames]


def find_shared(obj):
    from .machinetypes import Pair, Vector, shareable_types

    seen = set()
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
        seen.add(obj)

        if isinstance(obj, Pair):
            stack.append(obj.car)
            stack.append(obj.cdr)
        elif isinstance(obj, Vector):
            for e in obj:
                stack.append(e)
        else:
            assert False

    return shared
