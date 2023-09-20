from importlib.abc import Traversable
import os
from importlib import resources
from pathlib import Path
import shutil

from .version import __version__


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


def init_stdlib():
    from .config import Config  # avoid circular import

    cfg = Config()
    if not cfg.dev_mode:
        copy_lib_dir()

    update_cache()


def copy_lib_dir():
    from .config import Config  # avoid circular import

    cfg = Config()
    cfg.lib_dir.mkdir(parents=True, exist_ok=True)

    version = None
    version_file = cfg.lib_dir / 'version'
    if version_file.exists():
        with open(version_file, 'r') as f:
            version = f.read().strip()

    if version == __version__:
        return

    for i in resources.files('trick.scm').iterdir():
        if i.is_dir():
            dest_dir = cfg.lib_dir / i.name
            if dest_dir.exists():
                shutil.rmtree(dest_dir)
            copy_dir(i, dest_dir)
        else:
            copy_file(i, cfg.lib_dir / i.name)

    with open(version_file, 'w') as f:
        f.write(__version__ + '\n')


def copy_file(file: Traversable, dest: Path):
    with open(dest, 'wb') as f:
        f.write(file.read_bytes())


def copy_dir(src: Traversable, dest: Path):
    dest.mkdir(parents=True, exist_ok=True)
    for i in src.iterdir():
        if i.is_dir():
            copy_dir(i, dest / i.name)
        else:
            copy_file(i, dest / i.name)


def update_cache():
    from .config import Config  # avoid circular import
    from .libloader import LibLoader

    cfg = Config()
    cfg.cache_dir.mkdir(parents=True, exist_ok=True)

    update_fasl('stdlib', [])
    update_fasl('srfi-8', [cfg.cache_dir / 'stdlib.fasl'])

    LibLoader().add_fasl(get_builtin_fasl('stdlib'))
    LibLoader().add_fasl(get_builtin_fasl('srfi-8'))


def update_fasl(name: str, lib_fasls: list[Path]):
    from .config import Config  # avoid circular import

    cfg = Config()

    src_dir = cfg.lib_dir / name
    src_files = list(src_dir.glob('**/*.scm'))
    src_modified_time = max(i.stat().st_mtime for i in src_files)

    fasl_filename = cfg.cache_dir / (name + '.fasl')
    if fasl_filename.exists() and \
       fasl_filename.stat().st_mtime > src_modified_time:
        return

    # (re-)build the fasl
    compile_src_file_to_fasl(
        input_filename=src_dir / 'lib.scm',
        output_filename=fasl_filename,
        libs=lib_fasls,
    )


def get_builtin_fasl_filename(name: str):
    from .config import Config  # avoid circular import
    cfg = Config()

    # NOTE When adding new packages here, don't forget to also update:
    #  - get_builtin_fasl
    #  - get_builtin_fasl_filename
    #  - get_all_builtin_fasls
    #  - get_all_builtin_libs
    if name in ['stdlib', 'srfi-8']:
        return cfg.cache_dir / f'{name}.fasl'
    else:
        raise ValueError(f'Unknown built-in package: {name}')


def get_builtin_fasl(name: str):
    # TODO the fasls created by this function should somehow be cached
    filename = get_builtin_fasl_filename(name)
    return load_fasl_file(filename)


def get_all_builtin_fasls():
    return [
        get_builtin_fasl('stdlib'),
        get_builtin_fasl('srfi-8'),
    ]


def get_all_builtin_libs():
    from .libname import LibraryName

    return [
        LibraryName.create('trick', 'core'),
        LibraryName.create('trick'),
        LibraryName.create('scheme', 'base'),
        LibraryName.create('scheme', 'case-lambda'),
        LibraryName.create('scheme', 'char'),
        LibraryName.create('scheme', 'complex'),
        LibraryName.create('scheme', 'cxr'),
        LibraryName.create('scheme', 'eval'),
        LibraryName.create('scheme', 'file'),
        LibraryName.create('scheme', 'inexact'),
        LibraryName.create('scheme', 'lazy'),
        LibraryName.create('scheme', 'load'),
        LibraryName.create('scheme', 'process-context'),
        LibraryName.create('scheme', 'read'),
        LibraryName.create('scheme', 'repl'),
        LibraryName.create('scheme', 'time'),
        LibraryName.create('scheme', 'write'),
        LibraryName.create('srfi', 8),
    ]
