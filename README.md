# Trick

Trick is a small scheme-like language. The compiler targets an [SECD
machine][1]. It has lexical scoping and first-class continuations (via
`call/cc`) as any honest-to-god scheme should. It is however different from
normal schemes in the following ways:

 - Instead of hygienic macros, we have only `define-macro` which defines
   unhygienic macros.
 - Pairs and imrproper lists are not supported. We only have proper lists.
 - The only numeric type supported is big integers (they are basically more or
   less the same as Python integers).
 - Keyword symbols are supported (that is, symbols beginning with a colon
   evaluate to themselves).
 - The symbol `nil` evaluates to an empty list.

The code consists of the following main parts:

 - `compile.py`: Read source code and output SECD instructions in s-expr form.
 - `assemble.py`: Read SECD instructions in s-expr text form and output binary
   SECD instructions.
 - `secd.py`: Read binary SECD instructions and execute them.
 - `stdlib.lisp`: A small set of utility functions and macros provided as the
   standard library.
 - `test.py`: Run the test suite.
 - `repl.py`: Run a Read-Eval-Print loop. Currently rather limited, since state
   is not kept between two entered expressions, so you cannot first define a
   function and then call it.
   
If you have a source file named "foo.lisp" you can run it by:

    python3 compile.py -l stdlib.lisp foo.lisp | python3 assemble.py | python3 secd.py
    
Or you can use the provided "run.sh" script which does the same thing:

    ./run.sh foo.lisp

[1]: https://en.wikipedia.org/wiki/SECD_machine
