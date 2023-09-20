import os
import platform
import sys


from .version import __version__


def get_features():
    architecture = platform.machine().replace('_', '-')

    extra_features = []
    if sys.platform.startswith('linux'):
        os_name = 'gnu-linux'
        extra_features.append('linux')
    elif sys.platform.startswith('freebsd'):
        os_name = 'freebsd'
        extra_features.append('bsd')
    elif sys.platform == 'win32':
        os_name = 'windows'
    else:
        os_name = sys.platform
        if os_name[-1].isnumeric():
            os_name = os_name[:-1]

    if sys.byteorder == 'little':
        endianness = 'little-endian'
    else:
        endianness = 'big-endian'

    features = [
        'r7rs',
        'trick',
        'trick-' + __version__[:__version__.rindex('.')], # only add major and minor versions
        'full-unicode',
        'exact-closed',
        'exact-complex',
        'ieee-float',
        'ratios',
        architecture,
        os_name,
        endianness,
    ]
    features += extra_features

    if os.name == 'posix':
        features.append('posix')

    return features
