# Trick

Trick is a small scheme-like language. The compiler targets an [SECD
machine][1]. It has lexical scoping and first-class continuations (via
`call/cc`) as any honest-to-god scheme should. It is however different from
normal schemes in (at least) the following ways:

 - Instead of hygienic macros, we have only `define-macro` which defines
   unhygienic macros.
 - The only numeric type supported is big integers (they are basically more or
   less the same as Python integers).
 - Keyword symbols are supported (that is, symbols beginning with a colon
   evaluate to themselves).
 - Some tail calls are eliminated, but not all of them.
 - `define` and `set!` have a return value (the same value being written to the
   variable).
 - Returning multiple values is not supported
 - Many utilities are missing.
 - Libraries not supported.

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

## Lisp Primitives

Special forms:

 - `define`: Define a variable or functions. At the top-level, sets the given
   value/function to a symbol. At a local scope (let/letrec/lambda), expands the
   current local scope to add another local variable.
 - `define-macro`: Only allowed at the top-level. Defines a non-hygienic macro.
 - `set!`: Set a symbol or local variable to the given value.
 - `if`: Conditional statement. Can have one or two arms. If only one arm is
   specified, the other one (the false branch) returns `()`.
 - `lambda`: Creates a closure.
 - `let`: Create a set of local variables.
 - `letrec`: Like `let` but the variables are available inside the bindings too
   (so functions declared in bindings can be mutually recursive).
 - `quote`: Return the given value unevaluated.
 - `error`: Raise an error and stop execution. At least a single symbol is
   supposed to be passed to the function which will be interpreted as error
   type. An error message can also specified by adding `:msg "error message"` to
   the arguments.
 - `apply`: Apply a function to a list.
 - `call/cc`: Call the given function and pass the current continuation as its
   argument.

Primitive functions:

 - `iadd`: Add two integers
 - `isub`: Subtract two integers
 - `imul`: Multiply two integers
 - `idiv`: Divide two integers and return the quotient
 - `irem`: Divide two integers and return the remainder
 - `shr`: Logical shift right
 - `shl`: Logical shift left
 - `asr`: Arithmetic shift right
 - `b-not`: Binary NOT
 - `b-and`: Binary AND
 - `b-or`: Binary OR
 - `b-xor`: Binary XOR
 - `<`: Less-than operator
 - `<=`: Less-than-or-equal to operator
 - `print`: Print a value
 - `printc`: Print a character given its code
 - `halt`: Stop execution. Accepts a single argument as the exit code.
 - `cons`: Create a pair object
 - `car`: Return the "car" of a pair
 - `cdr`: Return the "cdr" of a pair
 - `type`: Return a symbol specifying the type of its argument. Possible return
   values are: `nil`, `symbol`, `pair`, `int`, `string`, `closure`, `bool`.
 - `eq?`: Return true if the two values are the exact same object.
 - `gensym`: Return a unique symbol
 - `apply`
 - `call/cc`
 - `call-with-current-continuation`
 - `char->integer`
 - `integer->char`
 - `char-general-category`
 - `char-upcase`
 - `char-downcase`
 - `char-foldcase`
 - `digit-value`
 - `make-string`
 - `string-ref`
 - `string-set!`
 - `string-length`

## SECD Instructions

The following instructions are supported by the SECD machine. The ones that are not present in the original SECD machine, or are not obvious from their names are accompanied by a short explanation.

 - `nil`
 - `true`
 - `false`
 - `cons`
 - `car`
 - `cdr`
 - `join`
 - `ap`
 - `ret`
 - `tap`: Tail apply. Like `ap` but replaces the current stack frame instead of
   pushing a new one on top of it.
 - `dum`
 - `rap`
 - `print`
 - `printc`
 - `halt`
 - `iadd`
 - `isub`
 - `imul`
 - `idiv`
 - `irem`
 - `shr`
 - `shl`
 - `asr`
 - `bnot`
 - `band`
 - `bor`
 - `bxor`
 - `lt`: Less than
 - `le`: Less than or equal to
 - `eq`
 - `drop`
 - `dup`
 - `xp`: Expand current environment. Used by `define` in local scopes.
 - `type`
 - `error`
 - `gensym`
 - `ccc`: call/cc
 - `i2ch`
 - `ch2i`
 - `ugcat`
 - `chup`
 - `chdn`
 - `chfd`
 - `chdv`
 - `mkstr`
 - `strref`
 - `strset`
 - `strlen`
 - `setcar`
 - `setcdr`
 - `ldc`
 - `ld`
 - `sel`
 - `ldf`
 - `st`: Like `ld` but writes a local variable, instead of reading it. Value
   read from the stack.
 - `ldfx`: Like `ldf`, but accepts a list of two. The second value is the same
   as the argument to `ldf`. The first value is an integer specifying the number
   of non-rest arguments the function receives.
 - `strtab`: List of literal strings used in the program.
 - `symtab`: List of symbols in the program. Each symbol is an index into the
   string table specified by `strtab`.
 - `ldstr`: Load a string literal onto stack by its index into the string table.
 - `ldsym`: Load a symbol onto stack by its index into the symbol table.
 - `set`: Set the value of a symbol.
 - `get`: Reads the value of a symbol.

[1]: https://en.wikipedia.org/wiki/SECD_machine
