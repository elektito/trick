#!/usr/bin/env sh

python3 compile.py -l stdlib.scm $1 | python3 assemble.py | python3 secd.py
