#!/usr/bin/env sh

python3 compile.py -l stdlib.lisp $1 | python3 assemble.py | python3 secd.py
