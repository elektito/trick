;;;; bitwise-core, core bitwise operations
;;; Copyright (C) 1991, 1993, 2001, 2003, 2005 Aubrey Jaffer
;;; This implementation of the seven core functions required by SRFI 33
;;; (bitwise-not, bitwise-and, bitwise-ior, bitwise-xor, arithmetic-shift,
;;; bit-count, integer-length) is drawn from the SRFI 60 implementation.
;;; Here is Shivers's comment on this code in SRFI 33:

;;; The [following] implementations of these functions use [only] R4RS
;;; arithmetic, so a simple-minded implementation again doesn't need to
;;; do much to support them -- however, [these] general implementations
;;; are terribly inefficient relative to native support and should *not*
;;; be used except in case of dire emergency. (It's quite clever code,
;;; nonetheless, to provide the semantics with such little support.)

;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.


(define (bitwise-not n) (#$bnot n))

(define (logical:reduce op ident args)
  (do ((res ident (op res (car rgs)))
       (rgs args (cdr rgs)))
      ((null? rgs) res)))

(define (bitwise-and . args)
  (logical:reduce #$band -1 args))

(define (bitwise-ior . args)
  (logical:reduce #$bor 0 args))

(define (bitwise-xor . args)
  (logical:reduce #$bxor 0 args))

(define (arithmetic-shift n count)
  (if (negative? count)
      (#$asr n (- count))
      (#$shl n count)))

(define (integer-length n)
  (#$intlen n))

(define (bit-count n)
  (#$popcnt n))
