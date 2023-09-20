# Trick

Trick is a full [R7RS-small][2] compliant implementation of the Scheme
programming language, targeting an [SECD machine][1]. It's written in Python
and, as of this moment, is very very slow! It's mostly an educational project at
the moment, though I might be able to write a faster implementation of the
virtual machine in the future.

## Requirements

Python 3.9 or later is required. There is no other dependency.

## Running Trick

In order to get a REPL, simply run `./trick.sh`. In order to compile a source
file named 'foo.scm' into a FASL file that can be run by the VM run `./trick.sh
-c foo.scm`. This creates a files named `foo.fasl`. Now you can run the file by
running `./trick.py run foo.fasl`.

[1]: https://en.wikipedia.org/wiki/SECD_machine
[2]: https://small.r7rs.org
