(import (trick core))

(define-library (trick)
  (import (trick core))

  ;; include the actual library source code
  (include "stdlib.scm")

  ;; include exports
  (include-library-declarations "trick-exports.scm"))

(define-library (trick repl)
  (import (trick))
  (include-library-declarations "trick-exports.scm")
  (export import)
  (include "trick-repl.scm"))

(define-library (scheme base)
  (import (trick))
  (include-library-declarations "scheme-base-exports.scm"))

(define-library (scheme case-lambda)
  (import (trick))
  (include-library-declarations "scheme-case-lambda-exports.scm"))

(define-library (scheme char)
  (import (trick))
  (include-library-declarations "scheme-char-exports.scm"))

;; (define-library (scheme complex)
;;   (import (trick))
;;   (include-library-declarations "scheme-complex-exports.scm"))

(define-library (scheme cxr)
  (import (trick))
  (include-library-declarations "scheme-cxr-exports.scm"))

;; (define-library (scheme eval)
;;   (import (trick))
;;   (include-library-declarations "scheme-eval-exports.scm"))

(define-library (scheme file)
  (import (trick))
  (include-library-declarations "scheme-file-exports.scm"))

(define-library (scheme inexact)
  (import (trick))
  (include-library-declarations "scheme-inexact-exports.scm"))

(define-library (scheme lazy)
  (import (trick))
  (include-library-declarations "scheme-lazy-exports.scm"))

;; (define-library (scheme load)
;;   (import (trick))
;;   (include-library-declarations "scheme-load-exports.scm"))

(define-library (scheme process-context)
  (import (trick))
  (include-library-declarations "scheme-process-context-exports.scm"))

(define-library (scheme read)
  (import (trick))
  (include-library-declarations "scheme-read-exports.scm"))

;; (define-library (scheme repl)
;;   (import (trick))
;;   (include-library-declarations "scheme-repl-exports.scm"))

;; (define-library (scheme time)
;;   (import (trick))
;;   (include-library-declarations "scheme-time-exports.scm"))

(define-library (scheme write)
  (import (trick))
  (include-library-declarations "scheme-write-exports.scm"))

(define-library (scheme r5rs)
  (import (trick))
  (include-library-declarations "scheme-r5rs-exports.scm"))
