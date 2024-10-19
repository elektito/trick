# Trick

Trick is a full [R7RS-small][2] compliant implementation of the Scheme
programming language, targeting an [SECD machine][1]. It's written in Python
and, as of this moment, is very very slow! It's mostly an educational project at
the moment, though I might be able to write a faster implementation of the
virtual machine in the future.

## SRFI Support

The following SRFIs are supported:

 - [SRFI 1][3]: List Library
 - [SRFI 8][4]: receive: Binding to multiple values
 - [SRFI 151][5]: Bitwise Operations 

## Requirements

Python 3.9 or later is required. There is no other dependency.

## Running Trick

In order to get a REPL, simply run `./trick.sh`. In order to compile a source
file named 'foo.scm' into a FASL file that can be run by the VM run `./trick.sh
-c foo.scm`. This creates a file named `foo.fasl`. Now you can run the file by
running `./trick.sh run foo.fasl`.

## Known Issues

 - `equal?` does not work correctly with circular lists.
 - `map` does not work correctly with circular lists.
 - The following outputs "oops!" while it shouldn't: `(let ((else #f)) (cond (else (print "oops!"))))`

[1]: https://en.wikipedia.org/wiki/SECD_machine
[2]: https://small.r7rs.org
[3]: https://srfi.schemers.org/srfi-1/srfi-1.html
[4]: https://srfi.schemers.org/srfi-8/srfi-8.html
[5]: https://srfi.schemers.org/srfi-151/srfi-151.html
