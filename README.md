# Trick

Trick is an ongoing implementation of the Scheme programming language, targeting
an [SECD machine][1], and aiming for full [R7RS-small compatibility][2]. It's written
in Python and, as of this moment, very very slow! It's mostly an educational
project at the moment, though I might be able to write a faster implementation
of the virtual machine in the future.

## Completion Status

The core language primitives are all implemented, but the internal libraries are
not complete yet. `(scheme base)` however is nearly complete however, as well as
several others.

## Requirements

Python 3.9 or later is required. There is no other dependency.

## Running Trick

In order to get a REPL, simply run `./trick.py`. In order to compile a source
file named 'foo.scm' into a FASL file that can be run by the VM run `./trick.py
-c foo.scm`. This creates a files named `foo.fasl`. Now you can run the file by
running `./trick.py run foo.fasl`.

[1]: https://en.wikipedia.org/wiki/SECD_machine
[2]: https://small.r7rs.org
