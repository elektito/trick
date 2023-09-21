(include-library-declarations "scheme-base-exports.scm")
(include-library-declarations "scheme-case-lambda-exports.scm")
(include-library-declarations "scheme-char-exports.scm")
(include-library-declarations "scheme-complex-exports.scm")
(include-library-declarations "scheme-cxr-exports.scm")
(include-library-declarations "scheme-eval-exports.scm")
(include-library-declarations "scheme-file-exports.scm")
(include-library-declarations "scheme-inexact-exports.scm")
(include-library-declarations "scheme-lazy-exports.scm")
(include-library-declarations "scheme-load-exports.scm")
(include-library-declarations "scheme-process-context-exports.scm")
(include-library-declarations "scheme-read-exports.scm")
(include-library-declarations "scheme-repl-exports.scm")
(include-library-declarations "scheme-time-exports.scm")
(include-library-declarations "scheme-write-exports.scm")

(export 1-
        1+
        all?
        any?
        approx=
        atom?
        box
        box?
        char-general-category
        define-macro
        e
        gensym
        iota
        let/cc
        list*
        null-environment
        scheme-report-environment
        pairwise
        pi
        print
        proper-length
        range
        set-box!
        type
        unbox
        void
        void?
        with-gensyms)
