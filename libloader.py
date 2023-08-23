class LibraryLoadError(Exception):
    pass


class LibLoader:
    _instance = None

    def __new__(cls, *args, **kwargs):
        if cls._instance is None:
            cls._instance = object.__new__(cls, *args, **kwargs)
            cls._instance.initialized = False

        return cls._instance

    def __init__(self):
        if self.initialized:
            return
        self.initialized = True

        # avoid circular reference
        from library import CoreLibrary

        self.libs = {}
        self.available_lib_info = []
        self.loading_in_progress = set()
        self.fasls = []

        self._core = CoreLibrary()
        self.libs[self._core.name] = self._core

    def get_core(self):
        return self._core

    def load_lib(self, lib_name):
        if lib_name in self.libs:
            return self.libs[lib_name]

        if lib_name in self.loading_in_progress:
            raise LibraryLoadError(
                f'Circular reference loading library: {lib_name}')

        self.loading_in_progress.add(lib_name)
        lib = self._load_lib(lib_name)
        self.loading_in_progress.remove(lib_name)

        if lib is not None:
            return lib

        raise LibraryLoadError(f'Cannot find library: {lib_name}')

    def _load_lib(self, lib_name):
        for fasl in self.fasls:
            section = fasl.get_section('libinfo')
            if section is None:
                continue
            for lib in section.libs:
                if lib.name == lib_name:
                    for var in lib.defined_symbols.values():
                        if var.value:
                            var.value.env= lib
                    return lib

        return None

    def add_lib(self, lib):
        self.libs[lib.name] = lib

    def add_fasl(self, fasl):
        self.fasls.append(fasl)
