#!/usr/bin/env sh

./trick.py compile -l stdlib.scm $1 | ./trick.py assemble | ./trick.py run
