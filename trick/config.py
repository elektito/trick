import os
from pathlib import Path

from .version import __version__


true_values = ['true', 't', 'yes', 'y']


class Config:
    def __init__(self):
        self.dev_mode = os.environ.get('DEV_MODE') in true_values

        if self.dev_mode:
            self.lib_dir = Path('./trick/scm/')
        else:
            self.lib_dir = Path('~/.local/lib/trick').expanduser()

        self.cache_dir = Path('~/.cache/trick').expanduser()
        self.version = __version__
