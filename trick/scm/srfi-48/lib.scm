(import (trick core))

(define-library (srfi 48)
  (import (scheme base) (scheme write) (scheme complex) (scheme char) (only (trick core) void))
  (export format)
  (include "srfi-48.scm"))
