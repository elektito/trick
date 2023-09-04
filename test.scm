;; let's start by testing some comments

#;100
#;#f
#;(a
   b ;; foo
   c)

#|
testing nested comments
#|
still a comment #||#
|#
and still a comment
|#; a comment right after another

;; and now to more important things! |# <---make emacs scheme mode happy

;; defines

(= 200 (let ((x 10))
         (define x 200)
         x))

(equal? '(100 200 300 400)
        (let ()
          (define x 100)
          (begin
            (define y 200)
            (define z 300))
          (define w 400)
          (list x y z w)))

(equal? '(20 100  200)
        (let ((a 10))
          (define x 100)
          (begin
            (define y 200)
            (set! a 20))
          (list a x y)))

(equal? 100 (let ((x 10))
              (define x 100)
              x))

(= 201 (let ()
         (define (foo x) (+ 1 (bar x)))
         (define (bar x) (+ x 100))
         (foo 100)))

(let ()
  (define (is-even? n)
    (or (zero? 0)
        (is-odd? (- n 1))))
  (define (is-odd? n)
    (and (not (zero? n))
         (is-even? (- n 1))))
  (and (is-odd? 11)
       (is-even? 12)))

(= 20 (let ((x 10))
        (define x 20)
        x))

(= 30 (let ()
        (define x 10)
        (define y (+ x 20))
        y))

;;

(eq? 1 1)
(eq? 'foo 'foo)

(eq? #f '#f)
(eq? #t '#t)
(eq? 10 '10)
(eq? #\space #\space)
(eq? '() '())
(not (eq? '() 'nil))
(equal? "foo" '"foo")
(equal? #() '#())
(equal? #(a b) '#(a b))

(eq? '() '[])
(equal? '(a b) '[a b])

(eqv? #t #t)
(eqv? #f #f)
(eqv? 1 1)
(eqv? 'foo 'foo)
(eqv? #\space #\x20)
(eqv? '() '())
(let ((x "foo"))
  (eqv? x x))
(let ((x (lambda (a) a)))
  (eqv? x x))

(symbol=? 'foo 'foo)
(not (symbol=? 'foo 'bar))

(boolean=? #t #t)
(not (boolean=? #t #f))

(string=? "foo" (symbol->string 'foo))
(eq? 'bar (string->symbol "bar"))

(symbol? '||)
(string=? "" (symbol->string '||))
(eq? 'foo '|foo|)
(string=? "two words" (symbol->string '|two words|))
(string=? "\t\t" (symbol->string '|\t\x09;|))
(string=? "hello" (symbol->string '|h\x65;llo|))
(string=? "a\nb" (symbol->string '|a\nb|))
(eq? '|two words| '|two\x20;words|)
(symbol? 'lambda)
(string=? "!$%&*+-./:<=>?@^_~" (symbol->string '!$%&*+-./:<=>?@^_~))
(eq? '|\x9;\x9;| '|\t\t|)

(not (eq? 'abc 'AbC))
(eq? #!fold-case 'abc 'AbC #!no-fold-case)
(not (eq? 'abc 'AbC))

(let ((x '#0=(#1=(#0#) b . #1#)))
  (and (eq? (caar x) x)
       (eq? (car x) (cddr x))))
(let ((x '(1 #50=(a b) #50#)))
  (eq? (cadr x) (caddr x)))
(equal? '(a (b (c (d e))) f (d e) g)
        '(a (b (c #0=(d e))) f #0# g))
(let ((x '(#0=(1 2) 3 #0#)))
  (eq? (car x) (caddr x)))
(let ((x '(1 . #0=(2 . #0#))))
  (eq? (cdr x) (cddr x)))

(let ((x '(1 #0=(a) #1=#0# #1# 2)))
  (and (equal? x '(1 (a) (a) (a) 2))
       (eq? (cadr x) (caddr x))
       (eq? (caddr x) (cadddr x))))

(let ((x 100))
  (set! x 200)
  (eq? 200 x))

(let ((x 100))
  (define (foo x) (+ 900 x))
  (eq? 1000 (foo 100)))

(let ((ls '()))
  (let f ((x 5))
    (unless (= x 0)
      (set! ls (cons x ls))
      (f (1- x))))
  (equal? ls '(1 2 3 4 5)))

(eq? 100 (if #t 100 200))
(eq? 200 (if #f 100 200))
(eq? 100 (if 0 100 200))
(eq? 100 (if 1 100 200))
(eq? 100 (if "foo" 100 200))
(eq? 100 (if (lambda (x) x) 100 200))
(eq? 100 (if #() 100 200))
(eq? 100 (if #(1) 100 200))
(eq? 100 (if #t 100))
(eq? (#$void) (if #f 100))

(eq? 3 (when #t 1 2 3))
(eq? (#$void) (when #f 1 2 3))
(eq? (#$void) (unless #t 1 2 3))
(eq? 3 (unless #f 1 2 3))

(cond (else #t))
(= 200 (cond ((= 10 20) 50 100)
             ((= 5 5) 80 200)
             ((= 40 40) 300)))
(= 300 (cond (#f 100)
             (#f 200)
             (300)
             (else 400)))
(= 301 (cond (#f 100)
             (#f => (lambda (x) 200))
             (300 => (lambda (x) (+ x 1)))
             (else 400)))

(eq? 'composite
     (case (* 2 3)
       ((2 3 5 7) 'prime)
       ((1 4 6 8 9) 'composite)))

(eq? 'c
     (case (car '(c d))
       ((a e i o u) 'foo 'vowel)
       ((w y) 'bar 'semivowel)
       (else => (lambda (x) x))))

(= 200
   (case (* 2 3)
     ((2 3) 50 100)
     ((6 7 8) 60 200)))

(= 777
   (case (* 2 3)
     ((2 3) 100)
     ((7 8) => (lambda (x) (+ x 10)))
     (else 666 777)))

(= 600
   (case (* 2 3)
     ((2 3) 100)
     ((7 8) 200)
     (else => (lambda (x) (* x 100)))))

;; regression test: putting one-armed if in a lambda just to make sure the
;; "join" instruction is correctly generated for the implied "false" branch.
(eq? (#$void) ((lambda () (if #f 100))))

(eq? 0 (+))
(eq? 2 (+ 2))
(eq? 100 (+ 30 70))
(eq? 100 (+ 130 -30))
(eq? 25 (+ 2 5 7 11))

(eq? -2 (- 2))
(eq? -40 (- 30 70))
(eq? 160 (- 130 -30))
(eq? -21 (- 2 5 7 11))

(eq? 1 (*))
(eq? 10 (* 10))
(eq? 50 (* 10 5))
(eq? 300 (* 10 5 6))

(eq? 1 (/ 1))
(eq? 2 (/ 10 5))
(eq? 20 (/ 3000 10 5 3))

(eq? 2 (floor-quotient 8 3))
(eq? -3 (floor-quotient -8 3))
(eq? 2 (truncate-quotient 8 3))
(eq? -2 (truncate-quotient -8 3))

(eq? 1 (remainder 10 3))
(eq? -1 (remainder -10 3))
(eq? 1 (remainder 10 -3))
(eq? -1 (remainder -10 -3))

(eq? 1 (modulo 10 3))
(eq? 2 (modulo -10 3))
(eq? -2 (modulo 10 -3))
(eq? -1 (modulo -10 -3))

(eq? 7 (#$shr 29 2))
(eq? 116 (#$shl 29 2))
(eq? 7 (#$asr 29 2))
(eq? -8 (#$asr -29 2))

(eq? -1 (#$bnot 0))
(eq? 0 (#$bnot -1))
(eq? -2 (#$bnot 1))

(eq? 0 (#$band 0 0))
(eq? 0 (#$band 0 1))
(eq? 0 (#$band 1 0))
(eq? 1 (#$band 1 1))
(eq? #xaa00 (#$band #xaabb #xff00))
(eq? 0 (#$band 19999 0))
(eq? 19999 (#$band 19999 -1))

(eq? 0 (#$bor 0 0))
(eq? 1 (#$bor 0 1))
(eq? 1 (#$bor 1 0))
(eq? 1 (#$bor 1 1))
(eq? #xffbb (#$bor #xaabb #xff00))
(eq? 19999 (#$bor 19999 0))

(eq? 0 (#$bxor 0 0))
(eq? 1 (#$bxor 0 1))
(eq? 1 (#$bxor 1 0))
(eq? 0 (#$bxor 1 1))
(eq? 5 (#$bxor #xf #xa))

(eq? #f (not #t))
(eq? #t (not #f))
(eq? #f (not '()))
(eq? #f (not 'nil))
(eq? #f (not 1))
(eq? #f (not 0))

(< 10 20)
(not (< 20 10))
(not (< 10 10))
(< -10 1)
(< -20 0)

(<= 10 20)
(not (<= 20 10))
(<= 10 10)
(<= -10 1)
(<= -20 0)

(> 81 5)
(not (> 5 81))
(not (> 5 5))
(> 1 -50)
(> 0 -10)

(>= 81 5)
(not (>= 5 81))
(>= 10 10)
(>= 1 -10)
(>= 0 -20)

(zero? 0)
(not (zero? 1))
(not (zero? -1))

(negative? -1)
(not (negative? 0))
(not (negative? 1))

(positive? 1)
(not (positive? 0))
(not (positive? -1))

(even? 42)
(even? -100)
(even? 0)
(not (even? 101))
(not (even? -1))

(odd? 43)
(odd? -99)
(not (odd? 0))
(not (odd? 2))
(not (odd? -100))

(= 2 (max 1 2))
(= 8 (max 4 1 8 7 -9))

(= 1 (min 1 2))
(= -9 (min 4 1 8 7 -9))

(eq? 10 ((lambda (x y) x) 10 20))
(eq? 20 ((lambda (x y) y) 10 20))

(eq? 3 (and 1 2 3))
(eq? #f (and 1 2 #f 3))
(eq? 1 (or 1 2 3))
(eq? 1 (or #f #f 1 2 3))

(let ((f (lambda (n)
           (lambda (x)
             (set! n (+ n x))
             n))))
  (let ((acc1 (f 100))
        (acc2 (f 200)))
    (and (eq? (acc1 0) 100)
         (eq? (acc2 0) 200)
         (eq? (acc1 10) 110)
         (eq? (acc2 1) 201)
         (eq? (acc1 20) 130)
         (eq? (acc2 2) 203))))

(letrec ((is-even? (lambda (n)
                       (or (eq? n 0)
                           (is-odd? (#$isub n 1)))))
         (is-odd? (lambda (n)
                    (and (not (eq? n 0))
                         (is-even? (#$isub n 1))))))
    (is-odd? 11))

(letrec ()
  #t)

(letrec ((fib (lambda (n)
                (if (< n 2)
                    n
                    (+ (fib (- n 1)) (fib (- n 2)))))))
  (eq? 55 (fib 10)))

(eq? 12 (let* ((x 10)
               (y (+ x 2)))
          y))

(eq? 100 (car (cons 100 '())))
(eq? 'bar (cadr '(foo bar spam eggs)))

;; type predicates

(eq? 'nil (#$type '()))
(eq? 'pair (#$type '(1 2)))
(eq? 'int (#$type 42))
(eq? 'string (#$type "foo"))
(eq? 'char (#$type #\space))
(eq? 'procedure (#$type (lambda (x) x)))
(eq? 'bool (#$type #f))
(eq? 'bool (#$type #t))
(eq? 'vector (#$type #()))
(eq? 'vector (#$type #(1)))

(null? '())
(not (null? '(1)))
(not (null? '(1 2)))
(not (null? '(1 . 2)))
(not (null? 1))
(not (null? "foo"))
(not (null? 'foo))
(not (null? (lambda (x) x)))
(not (null? #()))
(not (null? #(1 2)))

(pair? '(1))
(pair? '(1 2))
(pair? '(1 2 3))
(pair? '(1 . 2))
(not (pair? '()))
(not (pair? 'foo))
(not (pair? "foo"))
(not (pair? 1))
(not (pair? (lambda (x) x)))
(not (pair? #(1)))

(list? '())
(list? '(1))
(list? '(1 2))
(not (list? '(1 . 2)))
(not (list? '(1 2 . 3)))
(not (list? (lambda (x) x)))
(not (list? #(1)))

(symbol? 'foo)
(not (symbol? '()))
(not (symbol? 1))
(not (symbol? "foo"))
(not (symbol? '(1 . 2)))
(not (symbol? '(1 2)))
(not (symbol? (lambda (x) x)))
(not (symbol? #(1)))

(boolean? #f)
(boolean? #t)
(not (boolean? -1))
(not (boolean? 0))
(not (boolean? 1))
(not (boolean? 'foo))
(not (boolean? '()))
(not (boolean? '(1 2)))
(not (boolean? (lambda (x) x)))
(not (boolean? #(1)))

(integer? -1)
(integer? 0)
(integer? 1)
(integer? #x10)
(integer? #o10)
(integer? #b10)
(integer? #d10)
(not (integer? 'foo))
(not (integer? "foo"))
(not (integer? '()))
(not (integer? '(1)))
(not (integer? '(1 2)))
(not (integer? '(1 . 2)))
(not (integer? (lambda (x) x)))
(not (integer? #(1)))

(string? "")
(string? "foo")
(not (string? 'foo))
(not (string? 1))
(not (string? '()))
(not (string? '(1)))
(not (string? '(1 2)))
(not (string? '(1 . 2)))
(not (string? (lambda (x) x)))
(not (string? #(1)))

(procedure? (lambda (x) x))
(procedure? (lambda () 10))
(not (procedure? 'foo))
(not (procedure? 1))
(not (procedure? "foo"))
(not (procedure? '()))
(not (procedure? '(1)))
(not (procedure? '(1 2)))
(not (procedure? '(1 . 2)))
(not (procedure? #(1)))

(char? #\a)
(char? #\tab)
(char? #\x09)
(not (char? '()))
(not (char? '(1)))
(not (char? '(1 . 2)))
(not (char? "f"))
(not (char? 'f))
(not (char? #(1)))
(not (char? (lambda (x) x)))

(vector? #())
(vector? #(1))
(not (vector? '()))
(not (vector? '(1)))
(not (vector? 'foo))
(not (vector? 1))
(not (vector? "foo"))
(not (vector? (lambda (x) x)))
(not (vector? #\a))

;; utility

(equal? (pairwise list '(1 2 3 4)) '((1 2) (2 3) (3 4)))

;; list

(equal? 1 (caar '((1) 2)))
(equal? 2 (cadr '(1 2)))
(equal? '(2) (cdar '((1 2) 3)))
(equal? '(3 4) (cddr '(1 2 3 4)))

(equal? 1 (caaar '(((1 2) 8 9) 20 30)))
(equal? 5 (caadr '(1 (5 6) 7 100)))
(equal? '(2 3) (cadar '((1 (2 3)) 100)))
(equal? 3 (caddr '(1 2 3 4)))
(equal? '(2 3) (cdaar '(((1 2 3) 50) 100)))
(equal? '(3) (cdadr '(1 (2 3) 4 5)))
(equal? '(3) (cddar '((1 2 3) 4)))
(equal? '(4 5) (cdddr '(1 2 3 4 5)))

(equal? 1 (caaaar '((((1 2) 80) 90) 100)))
(equal? 2 (caaadr '(1 ((2)) 3 4)))
(equal? '(2 3) (caadar '((1 ((2 3) 4) 5 6 7) 100)))
(equal? 3 (caaddr '(1 2 (3 10) 4 5)))
(equal? 2 (cadaar '(((1 2 3 4) 90) 100)))
(equal? 10 (cadadr '(1 (2 10 20 30) 3 4 5)))
(equal? 3 (caddar '((1 2 3 4 5) 100)))
(equal? 4 (cadddr '(1 2 3 4 5)))
(equal? '(2 3) (cdaaar '((((1 2 3) 80) 90) 100)))
(equal? '(20) (cdaadr '(1 ((10 20) 2) 3 4)))
(equal? '(3) (cdadar '((1 (2 3) 4 5) 6 7)))
(equal? '(10 20) (cdaddr '(1 2 (3 10 20) 4 5)))
(equal? '(3 4) (cddaar '(((1 2 3 4) 90) 100)))
(equal? '(20) (cddadr '(1 (2 10 20) 3)))
(equal? '(4) (cdddar '((1 2 3 4) 100)))
(equal? '(5 6) (cddddr '(1 2 3 4 5 6)))

(eq? 0 (length '()))
(eq? 1 (length '(1)))
(eq? 2 (length '(1 2)))
(eq? 3 (length '(1 2 3)))

(equal? (iota -1) '())
(equal? (iota 0) '())
(equal? (iota 5) '(0 1 2 3 4))
(equal? (range 0 0) '())
(equal? (range 0 5) '(0 1 2 3 4))
(equal? (range 5 0) '())
(equal? (range -5 -2) '(-5 -4 -3))

(equal? '() (reverse '()))
(equal? '(1) (reverse '(1)))
(equal? '(1 2 3) (reverse '(3 2 1)))
(equal? '(1 (2 3) 4 5) (reverse '(5 4 (2 3) 1)))

(equal? '(c d e f) (list-tail '(a b c d e f) 2))
(equal? '(a b c d e f) (list-tail '(a b c d e f) 0))

(equal? 'c (list-ref '(a b c d e f) 2))
(equal? 'a (list-ref '(a b c d e f) 0))

(equal? '(10 10 10 10) (make-list 4 10))
(equal? 10 (length (make-list 10)))

(null? (list-copy '()))
(equal? '(1 2 3) (list-copy '(1 2 3)))
(equal? '(1 2 3 . 4) (list-copy '(1 2 3 . 4)))
(let ((x #(1 2 3)))
  ;; non-list objects should be returned unchanged
  (eq? x (list-copy x)))

(let ((x '(1 2 3 4 5)))
  (list-set! x 2 'abc)
  (equal? '(1 2 abc 4 5) x))

(equal? '(3 4 5) (member 3 '(1 2 3 4 5)))
(not (member 30 '(1 2 3 4 5)))
(equal? '(1 2 3 4 5) (member 30 '(1 2 3 4 5) (lambda (x y) #t)))

;; equality

(not (eq? (gensym) (gensym)))
(let ((gs (gensym)))
  (eq? gs gs))

(equal? "foo" "foo")
(equal? #\A #\A)
(equal? 'foo 'foo)
(equal? 1 1)
(not (equal? 1 2))
(equal? '(1 foo 2) '(1 foo 2))
(not (equal? '(1 foo 2) '(1 foo 2 3)))
(equal? '(1 (foo) 2) '(1 (foo) 2))
(equal? '(1 (foo "bar") 2) '(1 (foo "bar") 2))
(not (equal? '(1 (foo "bar") 2) '(1 (foo "bar" 10) 2)))

(eq? 6 (apply + '(1 2 3)))
(eq? 10 (apply + '(1 2 3 4)))
(eq? 0 (apply + '()))
(equal? '(1 2 3 foo bar)
        (apply list 1 2 3 '(foo bar)))

(= 1)
(= 1 1)
(= 1 1 1)
(not (= 1 1 2 1))

;; characters
(eq? #\space #\ )
(eq? #\space #\x20)
(eq? #\x #\x78)
(eq? #\( #\x28)
(eq? #\) #\x29)
(eq? #\alarm #\x7)
(eq? #\backspace #\x8)
(eq? #\delete #\x7f)
(eq? #\newline #\xA)
(eq? #\null #\x0)
(eq? #\return #\x0d)
(eq? #\tab #\x09)
(eq? #\λ #\x03bb)

(char? #\space)
(char? #\A)
(char? #\x40)

(char=? #\space)
(char=? #\space #\space)
(char=? #\space #\  #\space)
(not (char=? #\A #\B))
(not (char=? #\A #\B #\space))

(char<? #\a)
(char<? #\a #\b)
(char<? #\a #\b #\c)
(char<? #\A #\a)
(not (char<? #\b #\a))
(not (char<? #\a #\a #\b #\c))

(char<=? #\a)
(char<=? #\a #\a)
(char<=? #\a #\b)
(char<=? #\a #\b #\c)
(char<=? #\A #\a)
(not (char<=? #\b #\a))
(char<=? #\a #\a #\b #\c)
(char<=? #\a #\b #\b #\c)

(char>? #\a)
(char>? #\b #\a)
(char>? #\c #\b #\a)
(char>? #\a #\A)
(not (char>? #\a #\b))
(not (char>? #\c #\b #\a #\a))

(char>=? #\a)
(char>=? #\a #\a)
(char>=? #\b #\a)
(char>=? #\c #\b #\a)
(char>=? #\a #\A)
(not (char>=? #\a #\b))
(char>=? #\c #\b #\a #\a)
(char>=? #\c #\b #\b #\a)

(eq? 'Lu (char-general-category #\A))
(eq? 'Ll (char-general-category #\a))
(eq? 'Nd (char-general-category #\5))
(eq? 'Nd (char-general-category #\۴))
(eq? 'Zs (char-general-category #\space))

(char-alphabetic? #\A)
(char-alphabetic? #\a)
(char-alphabetic? #\ف)
(not (char-alphabetic? #\1))
(not (char-alphabetic? #\۷))

(char-upper-case? #\A)
(not (char-upper-case? #\a))
(not (char-upper-case? #\ف))

(char-lower-case? #\a)
(not (char-lower-case? #\A))
(not (char-lower-case? #\ف))

(char-whitespace? #\space)
(char-whitespace? #\tab)
(char-whitespace? #\newline)
(char-whitespace? #\return)
(not (char-whitespace? #\a))
(not (char-whitespace? #\1))

(char-numeric? #\0)
(char-numeric? #\5)
(char-numeric? #\۵)
(not (char-numeric? #\A))
(not (char-numeric? #\space))
(not (char-numeric? #\tab))

(eq? #\A (char-upcase #\a))
(eq? #\A (char-upcase #\A))
(eq? #\1 (char-upcase #\1))
(eq? #\ف (char-upcase #\ف))

(eq? #\a (char-downcase #\a))
(eq? #\a (char-downcase #\A))
(eq? #\1 (char-downcase #\1))
(eq? #\ف (char-downcase #\ف))

(eq? #\a (char-foldcase #\a))
(eq? #\a (char-foldcase #\A))
(eq? #\1 (char-foldcase #\1))
(eq? #\ف (char-foldcase #\ف))

(eq? #f (digit-value #\a))
(eq? #f (digit-value #\A))
(eq? 0 (digit-value #\0))
(eq? 4 (digit-value #\4))
(eq? 4 (digit-value #\۴))

(char-ci=? #\A #\a)
(char-ci=? #\A #\A)
(char-ci=? #\a #\a)
(char-ci=? #\1 #\1)
(char-ci=? #\A #\a #\a)
(not (char-ci=? #\A #\b #\A))
(char-ci=? #\space)
(char-ci=? #\space #\space)
(char-ci=? #\space #\  #\space)
(not (char-ci=? #\A #\B))
(not (char-ci=? #\A #\B #\space))

(char-ci<? #\a)
(char-ci<? #\a #\b)
(char-ci<? #\a #\b #\c)
(not (char-ci<? #\A #\a))
(not (char-ci<? #\b #\a))
(not (char-ci<? #\a #\a #\b #\c))
(char-ci<? #\a #\B)

(char-ci<=? #\a)
(char-ci<=? #\a #\a)
(char-ci<=? #\A #\a)
(char-ci<=? #\a #\A)
(char-ci<=? #\a #\b)
(char-ci<=? #\a #\B)
(char-ci<=? #\a #\b #\c)
(char-ci<=? #\a #\b #\C)
(not (char-ci<=? #\b #\a))
(not (char-ci<=? #\B #\a))
(char-ci<=? #\a #\a #\b #\c)
(char-ci<=? #\a #\b #\b #\c)
(char-ci<=? #\a #\A #\b #\c)
(char-ci<=? #\a #\b #\B #\c)

(char-ci>? #\a)
(char-ci>? #\b #\a)
(char-ci>? #\B #\a)
(char-ci>? #\b #\A)
(char-ci>? #\c #\b #\a)
(char-ci>? #\c #\B #\a)
(not (char-ci>? #\a #\A))
(not (char-ci>? #\a #\b))
(not (char-ci>? #\A #\b))
(not (char-ci>? #\c #\b #\a #\a))
(not (char-ci>? #\c #\b #\A #\a))
(not (char-ci>? #\c #\B #\a #\a))

(char-ci>=? #\a)
(char-ci>=? #\a #\a)
(char-ci>=? #\a #\A)
(char-ci>=? #\A #\a)
(char-ci>=? #\b #\a)
(char-ci>=? #\B #\a)
(char-ci>=? #\b #\A)
(char-ci>=? #\c #\b #\a)
(char-ci>=? #\c #\B #\a)
(char-ci>=? #\C #\b #\a)
(not (char-ci>=? #\a #\b))
(not (char-ci>=? #\A #\b))
(not (char-ci>=? #\a #\B))
(char-ci>=? #\c #\b #\a #\a)
(char-ci>=? #\c #\b #\b #\a)
(char-ci>=? #\c #\B #\a #\a)
(char-ci>=? #\c #\b #\b #\A)

(eq? (char->integer #\A) 65)
(eq? (integer->char 32) #\space)

;; string

(string=? "Hello" "H\x65;llo")
(string=? "foobar" "foo\
                    bar")
(char=? #\tab (string-ref "\t" 0))
(char=? #\newline (string-ref "\n" 0))
(char=? #\return (string-ref "\r" 0))
(char=? #\alarm (string-ref "\a" 0))
(char=? #\backspace (string-ref "\b" 0))
(char=? #\" (string-ref "\"" 0))
(char=? #\x7c (string-ref "\|" 0))
(char=? #\null (string-ref "\x0;" 0))

(string=? (make-string 10 #\A) "AAAAAAAAAA")
(eq? 10 (string-length (make-string 10 #\A)))
(eq? 10 (string-length (make-string 10)))

(string=? "" (string))
(string=? "A" (string #\A))
(string=? "ABC" (string #\A #\B #\C))

(eq? #\A (string-ref "ABC" 0))
(eq? #\B (string-ref "ABC" 1))
(eq? #\C (string-ref "ABC" 2))

(let ((s (make-string 3 #\space)))
  (string-set! s 0 #\X)
  (string-set! s 2 #\Z)
  (string=? s "X Z"))

(equal? "cde" (substring "abcdefg" 2 5))
(equal? "" (substring "abcdefg" 4 4))

(equal? "cde" (string-copy "abcdefg" 2 5))
(equal? "" (string-copy "abcdefg" 4 4))
(equal? "cdefg" (string-copy "abcdefg" 2))
(let* ((s "abcd")
       (r (string-copy s)))
  (and (equal? s r)
       (not (eq? s r))))

(let ((s "1234567"))
  (string-copy! s 2 "ab")
  (equal? s "12ab567"))
(let ((s "1234567"))
  (string-copy! s 2 "abcde" 3)
  (equal? s "12de567"))
(let ((s "1234567"))
  (string-copy! s 3 "abcde" 2 4)
  (equal? s "123cd67"))

(equal? "" (string-append))
(let* ((s "12")
       (a (string-append s)))
  (and (not (eq? s a)) ;; the return value should be a newly allocated string
       (equal? "12" a)))
(equal? "123456" (string-append "12" "" "3456"))

(let ((s "12345"))
  (string-fill! s #\a)
  (equal? "aaaaa" s))
(let ((s "12345"))
  (string-fill! s #\a 2)
  (equal? "12aaa" s))
(let ((s "12345"))
  (string-fill! s #\a 2 4)
  (equal? "12aa5" s))

(equal? (string->list "") '())
(equal? (string->list "ABC") '(#\A #\B #\C))
(equal? (string->list "ABCDEFG" 2 5) '(#\C #\D #\E))
(equal? (string->list "ABCDEFG" 2) '(#\C #\D #\E #\F #\G))
(equal? (list->string '(#\A #\B #\C)) "ABC")
(equal? (list->string '()) "")

(let ((x '(1 2 3)))
  (set-car! x 10)
  (equal? x '(10 2 3)))

(let ((x '(1 2 3)))
  (set-cdr! x 10)
  (equal? x '(1 . 10)))

;; quasiquote tests

(eq? `() '())
(eq? `a 'a)
(eq? `,10 10)
(equal? '(a . b) `(a . b))
(equal? '(a b . c) `(a b . c))
(equal? '(a . 4) `(a . ,(+ 2 2)))
(equal? '(a b . 4) `(a b . ,(+ 2 2)))
(let ((x '(3 4)))
  (equal? `(1 2 ,@x 5)
          '(1 2 3 4 5)))
(let ((x '(3 4)))
  (equal? `(1 2 ((,@x)) 5)
          '(1 2 ((3 4)) 5)))
(let ((x 10) (y 20) (z 30))
  (equal? `(x ,y z) '(x 20 z)))
(let ((x 10) (y 20) (z 30))
  (equal? `(,x ,y ,z) '(10 20 30)))
(let ((x 10) (y 20) (z 30))
  (equal? `(x y z) '(x y z)))
(let ((x 10) (y 20) (z 30))
  (equal? `(x ((,y)) z) '(x ((20)) z)))
(let ((x 10) (y 20) (z 30))
  (equal? ```(x ,,,y z) '``(x ,,20 z)))
(equal? ``(a ,,(+ 1 2) ,(+ 2 3))
        '`(a ,3 ,(+ 2 3)))
(equal? ``,,3 '`,3)
(equal? ```,,,3 '``,,3)

;; the following are adopted from husk scheme test suite. see
;; https://github.com/justinethier/husk-scheme/blob/master/tests/t-backquote.scm
(equal? `(list ,(car '(3 6)) 4)
         '(list 3 4))
(equal? (let ((name 'a)) `(list ,name ',name))
        '(list a (quote a)))
(equal? (let ((name 'a)) '(list ,name ',name))
        '(list (unquote name) (quote (unquote name))))
(equal? (let ((name 'a)) `(list ,name (,name)))
        '(list a (a)))
(equal? (let ((name 'a)) `(list ,name ((,name))))
        '(list a ((a))))
(equal? `(a `(b ,(car '(3 6)) ,(foo ,(car '(3 6)) d) e) f)
        '(a `(b ,(car '(3 6)) ,(foo 3 d) e) f))
(equal? (let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,,name2 d) e))
        '(a `(b ,x ,y d) e))
(equal? (let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))
        '(a `(b ,x ,'y d) e))
(equal? (quasiquote (list (unquote (car '(3 6))) 4))
        '(list 3 4))
(equal? '(quasiquote (list (unquote (car '(3 6))) 4))
        '`(list ,(car '(3 6)) 4))
(equal? `(a `(b ,(foo ,(car '(3 6))) c) d)
        '(a `(b ,(foo 3) c) d))
(equal? '(x `(,@'(a b c)))
        `(x `(,@'(a b c))))
(equal? '`(,@(+ 1 1))
        ``(,@(+ 1 1)))
(equal? '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
        `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))

;; from chez scheme docs at: https://www.scheme.com/tspl2d/control.html
(equal? '(1 . 2)
        (let ((a 1) (b 2))
          `(,a . ,b)))
(equal? '(1 . 2)
        (let ((a 1) (b 2))
          `(,a ,@b)))
(equal? ''(a . b)
        `',(cons 'a 'b))

;; vectors and quasiquotes

(let ((square (lambda (x) (* x x))))
  (equal? #(10 5 4 16 9 8)
          `#(10 5 ,(square 2) ,@(map square '(4 3)) 8)))
(equal? `#(a `(b ,(foo ,(car '(3 6))) c) d)
        '#(a `(b ,(foo 3) c) d))

(let ((x 10) (y 20) (z 30))
  (equal? ```#(x ,,,y z) '``#(x ,,20 z)))
(equal? ``#(a ,,(+ 1 2) ,(+ 2 3))
        '`#(a ,3 ,(+ 2 3)))
(equal? `(x y #(z (w #(1 ,(+ 2 2)) a) b) c)
        '(x y #(z (w #(1 4) a) b) c))
(equal? ``(x y #(z (w #(1 ,,(+ 2 2)) a) b) c)
        '`(x y #(z (w #(1 ,4) a) b) c))
(equal? '(a b . #(1 2 3 4))
        `(a b . #(1 ,@(list 2 3) 4)))
(equal? '(1 `(2 . ,(+ 1 2)))
        `(1 `(2 . ,(+ 1 2))))
(equal? '(1 `(2 . ,3))
        `(1 `(2 . ,,(+ 1 2))))

;; vectors

(atom? #(1 2 3))
(vector? #(1 2 3))

(equal? '#(1 2 (a b) 3) #(1 2 (a b) 3))
(not (equal? #(1 2 '(a b) 3) #(1 2 (a b) 3)))

(equal? #(a a a a a) (make-vector 5 'a))
(= 5 (vector-length (make-vector 5)))

(= 0 (vector-length #()))
(= 3 (vector-length #(1 2 3)))
(= 3 (vector-length #0=#(1 2 #0#)))
(let ((v #0=#(1 2 #0#)))
  (eq? v (vector-ref v 2)))
(let ((v #(1 2 #0=(10) #0#)))
  (eq? (vector-ref v 2) (vector-ref v 3)))
(let ((v #0=#(1 (2 #0#) 3)))
  (eq? v (cadr (vector-ref v 1))))
(let ((v '#0=(1 #(2 #0#) 3)))
  (eq? v (vector-ref (cadr v) 1)))

(= 2 (vector-ref #(1 2 3) 1))

(let ((v #(1 2 3 4)))
  (vector-set! v 1 20)
  (equal? #(1 20 3 4) v))

(eq? '() (vector->list #()))
(equal? '(a b c) (vector->list #(a b c)))
(equal? '(1 2 #0=#(1 2 #0#)) (vector->list #1=#(1 2 #1#)))

(equal? #(41 62) (vector-map (lambda (x y z) (+ x y z))
                             #(1 2)
                             #(10 20)
                             #(30 40)))

(equal? #() (vector))
(equal? #(a b c) (vector 'a 'b 'c))

(equal? #() (string->vector ""))
(equal? #(#\1 #\2 #\3) (string->vector "123"))
(equal? #(#\c #\d #\e) (string->vector "abcde" 2))
(equal? #(#\c #\d) (string->vector "abcde" 2 4))

(equal? "" (vector->string #()))
(equal? "123" (vector->string #(#\1 #\2 #\3)))
(equal? "cde" (vector->string #(#\a #\b #\c #\d #\e) 2))
(equal? "cd" (vector->string #(#\a #\b #\c #\d #\e) 2 4))

(let ((v #(1 2 3 4 5)))
  (vector-fill! v 'a)
  (equal? #(a a a a a) v))
(let ((v #(1 2 3 4 5)))
  (vector-fill! v 'a 2)
  (equal? #(1 2 a a a) v))
(let ((v #(1 2 3 4 5)))
  (vector-fill! v 'a 2 4)
  (equal? #(1 2 a a 5) v))

(let* ((v #(1 2))
       (r (vector-copy v)))
  (and (not (eq? v r))
       (equal? v r)))
(equal? #(3 4 5) (vector-copy #(1 2 3 4 5) 2))
(equal? #(3 4) (vector-copy #(1 2 3 4 5) 2 4))

(let ((v #(1 2 3 4 5 6 7)))
  (vector-copy! v 2 #(a b))
  (equal? v #(1 2 a b 5 6 7)))
(let ((v #(1 2 3 4 5 6 7)))
  (vector-copy! v 2 #(a b c d e) 3)
  (equal? v #(1 2 d e 5 6 7)))
(let ((v #(1 2 3 4 5 6 7)))
  (vector-copy! v 3 #(a b c d e) 2 4)
  (equal? v #(1 2 3 c d 6 7)))

(equal? #() (vector-append))
(let* ((v #(1 2))
       (a (vector-append v)))
  (and (not (eq? v a)) ;; the return value should be a newly allocated vector
       (equal? #(1 2) a)))
(equal? #(1 2 3 4 5 6) (vector-append #(1 2) #() #(3 4 5 6)))

;; bytevectors

(atom? #u8(1 2 3))
(bytevector? #u8(1 2 3))

(equal? '#u8(1 2 3) #u8(1 2 3))

(equal? #u8(9 9 9 9 9) (make-bytevector 5 9))
(= 5 (bytevector-length (make-bytevector 5)))

(= 0 (bytevector-length #u8()))
(= 3 (bytevector-length #u8(1 2 3)))

(= 2 (bytevector-u8-ref #u8(1 2 3) 1))

(let ((v #u8(1 2 3 4)))
  (bytevector-u8-set! v 1 20)
  (equal? #u8(1 20 3 4) v))

(equal? #u8() (bytevector))
(equal? #u8(10 20 30) (bytevector 10 20 30))

(let* ((bv #u8(1 2))
       (r (bytevector-copy bv)))
  (and (not (eq? bv r))
       (equal? bv r)))
(equal? #u8(3 4 5) (bytevector-copy #u8(1 2 3 4 5) 2))
(equal? #u8(3 4) (bytevector-copy #u8(1 2 3 4 5) 2 4))

(let ((bv #u8(1 2 3 4 5 6 7)))
  (bytevector-copy! bv 2 #u8(10 20))
  (equal? bv #u8(1 2 10 20 5 6 7)))
(let ((bv #u8(1 2 3 4 5 6 7)))
  (bytevector-copy! bv 2 #u8(10 20 30 40 50) 3)
  (equal? bv #u8(1 2 40 50 5 6 7)))
(let ((bv #u8(1 2 3 4 5 6 7)))
  (bytevector-copy! bv 3 #u8(10 20 30 40 50) 2 4)
  (equal? bv #u8(1 2 3 30 40 6 7)))

(equal? #u8() (bytevector-append))
(let* ((bv #u8(1 2))
       (a (bytevector-append bv)))
  (and (not (eq? bv a)) ;; the return value should be a newly allocated bytevector
       (equal? #u8(1 2) a)))
(equal? #u8(1 2 3 4 5 6) (bytevector-append #u8(1 2) #u8() #u8(3 4 5 6)))

(equal? "ABCDE" (utf8->string #u8(#x41 #x42 #x43 #x44 #x45)))
(equal? "CDE" (utf8->string #u8(#x41 #x42 #x43 #x44 #x45) 2))
(equal? "BCD" (utf8->string #u8(#x41 #x42 #x43 #x44 #x45) 1 4))
(equal? #u8(#xCE #xBB) (string->utf8 "λ"))
(equal? #u8(216 167 216 179 218 169 219 140 217 133) (string->utf8 "اسکیم"))
(equal? #u8(218 169 219 140 217 133) (string->utf8 "اسکیم"
                                                        2))
(equal? #u8(216 179 218 169 219 140) (string->utf8 "اسکیم"
                                                   1 4))

;; case-lambda

(let ((f (case-lambda
          (() (list 'no-args))
          ((x) (list 'one-arg x))
          ((x y) (list 'two-args x y))
          ((x . r) (list 'one-arg-and-rest x r)))))
  (equal? '(no-args) (f)))

(let ((f (case-lambda
          (() (list 'no-args))
          ((x) (list 'one-arg x))
          ((x y) (list 'two-args x y))
          ((x . r) (list 'one-arg-and-rest x r)))))
  (equal? '(one-arg 100) (f 100)))

(let ((f (case-lambda
          (() (list 'no-args))
          ((x) (list 'one-arg x))
          ((x y) (list 'two-args x y))
          ((x . r) (list 'one-arg-and-rest x r)))))
  (equal? '(two-args 100 200) (f 100 200)))

(let ((f (case-lambda
          (() (list 'no-args))
          ((x) (list 'one-arg x))
          ((x y) (list 'two-args x y))
          ((x . r) (list 'one-arg-and-rest x r)))))
  (equal? '(one-arg-and-rest 100 (200 300)) (f 100 200 300)))

(let ((f (case-lambda
          (() (list 'no-args))
          ((x) (list 'one-arg x))
          ((x y) (list 'two-args x y))
          (r (list 'rest r)))))
  (equal? '(rest (100 200 300)) (f 100 200 300)))

;;

(let* ((ls '())
       (r (do ((i 0 (+ i 1))
               (j 3 (- j 1))
               (x 100))
              ((>= i 3)
               (set! ls (cons 'foo ls))
               (set! ls (cons 'bar ls))
               1000)
            (set! ls (cons (list i j x) ls)))))
  (and (= r 1000)
       (equal? ls '(bar foo (2 1 100) (1 2 100) (0 3 100)))))

(= 100 (do () (#t 100)))

;;

(equal? '(15 27 39)
        (map + '(1 2 3) '(4 5 6) '(10 20 30)))

(equal? '(15 27)
        (map + '(1 2) '(4 5 6) '(10 20 30)))

;; for-each (unlike map) is guaranteed to apply the function from left to right,
;; so we test it differently.
(let ((ls '()))
  (for-each
   (lambda x
     (set! ls (cons x ls)))
   '(1 2 3 4 5) '(10 20 30 40))
  (equal? '((4 40) (3 30) (2 20) (1 10)) ls))

(null? (append))
(equal? '(1 2 3 4) (append '(1) '(2 3) '(4)))
(equal? '(a) (append '() '(a)))
(equal? '(a b c . d) (append '(a b) '(c . d)))
(equal? '(a b c . d) (append '(a b) '(c) 'd))

(equal? '(b . 20) (assq 'b '((a . 10) (b . 20) (c . 30))))
(equal? #f (assq 'd '((a . 10) (b . 20) (c . 30))))
(equal? #f (assq 'a '()))

(equal? '(20 . b) (assv 20 '((10 . a) (20 . b) (30 . c))))
(equal? #f (assv 40 '((10 . a) (20 . b) (30 . c))))
(equal? #f (assv 10 '()))

(equal? '((b "bar") . 20)
        (assoc '(b "bar") '(((a "foo") . 10)
                            ((b "bar") . 20)
                            ((c "spam") . 30)
                            ((d "eggs") . 40))))

(equal? '((b "bar") . 20)
        (assoc '(b "bar")
               '(((a "foo") . 10)
                 ((b "bar") . 20)
                 ((c "spam") . 30)
                 ((d "eggs") . 40))
               equal?))

(equal? #f
        (assoc '(b "bar")
               '(((a "foo") . 10)
                 ((b "bar") . 20)
                 ((c "spam") . 30)
                 ((d "eggs") . 40))
               eqv?))

;; values

(equal? '(4 5)
        (call-with-values
            (lambda () (values 4 5))
          (lambda (a b) (list a b))))
(equal? '()
        (call-with-values
            (lambda () (values))
          (lambda x x)))
(equal? '(4 5)
        (call-with-values
            (lambda () (call/cc (lambda (k) (k 4 5))))
          (lambda (a b) (list a b))))
(equal? '()
        (call-with-values
            (lambda () (call/cc (lambda (k) (k))))
          (lambda x x)))
(= -1 (call-with-values * -))

(let*-values (((a b) (values 1 2))
              ((x y) (values a b)))
  (equal? '(1 2 1 2) (list a b x y)))

(let*-values (((a b) (values 1 2))
              (x (values a b)))
  (equal? '(1 2 (1 2)) (list a b x)))

(let-values (((x y) (values 1 2)))
  (and (= x 1)
       (= y 2)))

(let-values (((x y) (values 1 2))
             (foo (values 10 20))
             ((a b c) (values 3 4 5)))
  (and (= x 1)
       (= y 2)
       (= a 3)
       (= b 4)
       (equal? foo '(10 20))))

(let-values ((x (values 1 2)))
  (equal? x '(1 2)))

(let-values ((() (values)))
  #t)

(let ()
  (define (foo)
    (values 100 200))
  (define-values (x y) (foo))
  (and (= x 100)
       (= y 200)))

;; call/cc tests
;;
;; some tests adapted from chibi scheme test suite. see:
;; https://github.com/ashinn/chibi-scheme/blob/master/tests/r5rs-tests.scm
;; https://github.com/ashinn/chibi-scheme/blob/master/tests/r7rs-tests.scm

(eq? (call/cc #$type) 'procedure)
(eq? 30 (+ 10 (call/cc (lambda (k) (k 20)))))
(eq? 3 (call/cc (lambda (k) (+ 2 5 (k 3)))))
(eq? -3 (call/cc
         (lambda (exit)
           (map (lambda (x)
                  (if (negative? x)
                      (exit x)))
                '(54 0 37 -3 245 19))
           #t)))
(eq? 100 (call/cc (lambda (k) 100)))


;; make sure primitive functions are available as normal functions

(procedure? cons)
(equal? '(1 2) (apply cons '(1 (2))))

(procedure? car)
(equal? 1 (apply car '((1 2))))

(procedure? cdr)
(equal? '(2) (apply cdr '((1 2))))

(procedure? #$iadd)
(equal? 3 (apply #$iadd '(1 2)))

(procedure? #$isub)
(equal? -1 (apply #$isub '(1 2)))

(procedure? #$imul)
(equal? 2 (apply #$imul '(1 2)))

(procedure? #$idiv)
(equal? 2 (apply #$idiv '(4 2)))

(procedure? #$shr)
(equal? 1 (apply #$shr '(4 2)))

(procedure? #$shl)
(equal? 16 (apply #$shl '(4 2)))

(procedure? #$asr)
(equal? 1 (apply #$asr '(4 2)))

(procedure? #$bnot)
(equal? -1 (apply #$bnot '(0)))

(procedure? #$band)
(equal? 0 (apply #$band '(0 1)))

(procedure? #$bor)
(equal? 1 (apply #$bor '(0 1)))

(procedure? #$bxor)
(equal? 1 (apply #$bxor '(0 1)))

(procedure? #$ilt)
(apply #$ilt '(1 2))

(procedure? #$ile)
(apply #$ile '(1 1))

(procedure? #$type)
(equal? 'symbol (apply #$type '(x)))

(procedure? eq?)
(apply eq? '(foo foo))

(procedure? gensym)
(symbol? (apply gensym))

(procedure? char->integer)
(equal? 65 (apply char->integer '(#\A)))

(procedure? integer->char)
(equal? #\A (apply integer->char '(65)))

(procedure? char-upcase)
(equal? #\A (apply char-upcase '(#\a)))

(procedure? char-downcase)
(equal? #\a (apply char-downcase '(#\A)))

(procedure? char-foldcase)
(equal? #\a (apply char-foldcase '(#\A)))

(procedure? digit-value)
(equal? 1 (apply digit-value '(#\1)))

(procedure? make-string)
(equal? "AAA" (apply make-string '(3 #\A)))
(equal? 3 (string-length (apply make-string '(3))))

(procedure? string-ref)
(equal? #\C (string-ref "ABCD" 2))

(procedure? string-set!)
(let ((x (make-string 4 #\A)))
  (apply string-set! `(,x 2 #\X))
  (equal? "AAXA" x))

(procedure? string-length)
(equal? 4 (apply string-length '("AAAA")))

(procedure? call/cc)
(apply call/cc (list (lambda (k) (k #t))))

(procedure? call-with-current-continuation)
(apply call-with-current-continuation (list (lambda (k) (k #t))))

;; dynamic-wind

(equal?
 '(connect talk1 disconnect
   connect talk2 disconnect)
 (let ((path '())
       (c #f))
   (let ((add (lambda (s)
                (set! path (cons s path)))))
     (dynamic-wind
         (lambda () (add 'connect))
         (lambda ()
           (add (call/cc
                 (lambda (c0)
                   (set! c c0)
                   'talk1))))
         (lambda () (add 'disconnect)))
     (if (< (length path) 4)
         (c 'talk2)
         (reverse path)))))

(equal? '(x y z)
        (let* ((path '())
               (add (lambda (s) (set! path (cons s path)))))
          (dynamic-wind
              (lambda () (add 'x))
              (lambda () (add 'y))
              (lambda () (add 'z)))
          (reverse path)))

;; parameters

(let* ((results '())
       (add (lambda (x)
              (set! results (cons x results)))))
  (let ((p1 (make-parameter 10))
        (p2 (make-parameter 20 (lambda (x) (1+ x)))))
    (define out)
    (call/cc (lambda (k)
               (set! out k)))
    (add (p1))
    (add (p2))
    (parameterize ((p1 100) (p2 200))
      (add (p1))
      (add (p2))
      (when out
        (let ((k out))
          (set! out #f)
          (k #f)))
      (add (p1))
      (add (p2)))
    (add (p1))
    (add (p2)))
  (equal? '(10 21 100 201 10 21 100 201 100 201 10 21)
          (reverse results)))

;; record types

(let ()
  (define-record-type pare
    (kons x y)
    pare?
    (x kar set-kar!)
    (y kdr))
  (define r1 (kons 10 20))
  (define r2 (kons 1 2))
  (and (pare? r1)
       (pare? r2)
       (not (any? (map (lambda (pred) (pred r1))
                       (list integer? pair? vector? string?
                             symbol? null? boolean? char?
                             procedure?))))
       (= 10 (kar r1))
       (= 20 (kdr r1))
       (= 1 (kar r2))
       (= 2 (kdr r2))))

;; exceptions

(equal? '(handled foo)
        (call/cc (lambda (k)
                   (with-exception-handler
                    (lambda (e)
                      (k (list 'handled e)))
                    (lambda ()
                      (raise 'foo))))))

(equal? '(back handled foo)
        (with-exception-handler
         (lambda (e)
           (list 'handled e))
         (lambda ()
           (cons 'back (raise-continuable 'foo)))))

(let* ((results '())
       (add (lambda (x)
              (set! results (cons x results)))))
  (add (call/cc
        (lambda (k)
          (with-exception-handler
           (lambda (e)
             (add 'd)
             (k 'handled))
           (lambda ()
             (with-exception-handler
              (lambda (e)
                (add 'c))
              (lambda ()
                (add 'a)
                (raise 'foo)
                (add 'b))))))))
  (equal? '(handled d c a) results))


(let* ((results '())
       (add (lambda (x)
              (set! results (cons x results)))))
  (add (call/cc
        (lambda (k)
          (with-exception-handler
           (lambda (e)
             (add 'd)
             (add e)
             (k 'handled))
           (lambda ()
             (with-exception-handler
              (lambda (e)
                (add 'c)
                (add e)
                (raise 'bar))
              (lambda ()
                (add 'a)
                (raise 'foo)
                (add 'b))))))))
  (equal? '(handled bar d foo c a) results))

(call/cc (lambda (k)
           (with-exception-handler
            (lambda (e)
              (k #t))
            (lambda ()
              ;; cause a system exception
              (car '())
              ;; in case that somehow returned, cause the test to still fail!
              #f))))

(= 200 (with-exception-handler
        (lambda (e) 100)
        (lambda () 200)))

;; macros

(let-syntax ((swap! (syntax-rules ()
                      ((_ x y) (let ((tmp x))
                                 (set! x y)
                                 (set! y tmp))))))
  (let ((a 100) (b 200))
    (swap! a b)
    (and (= a 200) (= b 100))))

(letrec-syntax ((a (syntax-rules ()
                     ((_ x) 'A)
                     ((_ x y ...) (b y ...))))
                (b (syntax-rules ()
                     ((_ x) 'B)
                     ((_ x y ...) (a y ...)))))
  (and (eq? 'B (a 1 2 3 4 5 6))
       (eq? 'A (b 1 2 3 4 5 6))
       (eq? 'A (a 1 2 3 4 5))
       (eq? 'B (b 1 2 3 4 5))))

(let ()
  (define-syntax bind-to-zero
    (syntax-rules ()
      ((_ id) (define id 0))))
  (let ()
    (bind-to-zero x)
    (zero? x)))

(let-syntax ((q (syntax-rules ()
                  ((_ x) (quote x)))))
  (eq? 'abc (q abc)))

(let-syntax ((be-like-begin (syntax-rules ()
                              ((be-like-begin name)
                               (define-syntax name
                                 (syntax-rules ()
                                   ((name expr (... ...))
                                    (begin expr (... ...)))))))))
  (be-like-begin sequence)
  (= 4 (sequence 1 2 3 4)))

(let-syntax ((foo (syntax-rules ()
                    ((_) 100)
                    ((_ (z ...) (x y) ...)
                     (list x ... y ... '(x  z 100) ... 'z ...)) ((_ x) (+ x 1)))))
  (equal? '(1 3 5 2 4 6 (1 a 100) (3 b 100) (5 c 100) a b c)
          (foo (a b c) (1 2) (3 4) (5 6))))

;; test injecting local variables from the macro
(let ((x 10))
  (let ((x 100))
    (let-syntax ((foo (syntax-rules ()
                        ((_) (let ()
                               (let ()
                                 (list 10 20 x)))))))
      (let ((a 999)
            (x 200)
            (list (lambda x (list 'ABC x))))
        (equal? '(10 20 100) (foo))))))

;; test local macro expanding to "begin", used nested
(let ((x 10))
  (define-syntax my-begin
    (syntax-rules ()
      ((_ x ...) (begin x ...))))
  (my-begin
   (define y 20)
   (my-begin
    (define z 30)
    (set! x 100)))
  (equal? '(100 20 30) (list x y z)))

(let-syntax ((foo (syntax-rules ()
                    ((_ x) x))))
  (define (foo x) 200)
  (= 200 (foo 100)))

(let-syntax ((foo (syntax-rules ()
                    ((_ x) x))))
  (define (bar x) (foo x))
  (= 100 (bar 100)))

(let-syntax ((foo (syntax-rules ()
                    ((_) '(... ...)))))
  (equal? '... (foo)))

(let-syntax ((foo (syntax-rules ()
                    ((_ x ...) (... '(1 2 ... 3))))))
  (equal? '(1 2 ... 3) (foo)))

;; multiple splices
;;
;; for a while i wasn't sure if something like this is even allowed or not. but
;; after some discussions on reddit:
;;
;; https://www.reddit.com/r/scheme/comments/15sr4g9/multiple_ellipses_in_syntaxrules_pattern_language/
;;
;; looks like it is. what's more, i think i now understand _why_, even though i
;; still believe the report is very vague on this point. here's a snippet from
;; the report:
;;
;; > Pattern variables that occur in subpatterns followed by one or more instances
;; > of the identifier 〈ellipsis〉 are allowed only in subtemplates that are
;; > followed by as many instances of 〈ellipsis〉. They are replaced in the
;; > output by all of the elements they match in the input, distributed as
;; > indicated. It is an error if the output cannot be built up as specified.
;;
;; the my-append transformer down below matches the "subpatterns followed by one
;; or more instances of the identifier <ellipsis>". the part that says
;; "distributed as indicated" likely means that (a ... ...) is double-spliced
;; directly in the list, while ((a ...) ...) is spliced in sub-lists.

(equal? '(1 2 3 4 5 6)
        (let-syntax
            ((my-append
              (syntax-rules ()
                ((my-append (a ...) ...) '(a ... ...)))))
          (my-append (1 2 3) (4 5 6))))

(equal? '((1 2 3) (4 5 6))
        (let-syntax
            ((my-append
              (syntax-rules ()
                ((my-append (a ...) ...) '((a ...) ...)))))
          (my-append (1 2 3) (4 5 6))))

(equal? '(1 2 3 4 5 6 7 8)
        (let-syntax
            ((my-append
              (syntax-rules ()
                ((_ ((a ...) ...) ...) '(a ... ... ...)))))
          (my-append ((1 2) (3 4)) ((5) (6 7 8)))))

;; binding special keywords inside macro expansion
(let-syntax ((foo
              (syntax-rules ()
                ((_ x y ...)
                 (let ((if 1000))
                   (set! x if)
                   y ...)))))
  (let ((x 10))
    (foo x (if (> x 10) #t #f))))

;; rebinding identifiers that were already defined in transformer environment
;; inside expansion
(let-syntax ((foo
              (syntax-rules ()
                ((_ x)
                 (let ((list 1000))
                   (set! x list))))))
  (let ((x 10))
    (foo x)
    (= x 1000)))

(let ((x 10) (y 0))
  (let ((a 20) (x 100))
    (let-syntax ((foo (syntax-rules ()
                        ((_) (let ()
                               (let ()
                                 (set! x 999)))))))
      (let ((a 999)
            (x 200))
        (foo)))
    (set! y x))
  (and (= x 10) (= y 999)))

(let()
  (define result (let ((x 'outer))
                   (let-syntax ((m (syntax-rules () ((m) x))))
                     (let ((x 'inner))
                       (m)))))
  (eq? result 'outer))

(let ()
  (define result (let-syntax ((when (syntax-rules ()
                                      ((when test stmt1 stmt2 ...)
                                       (if test
                                           (begin stmt1
                                                  stmt2 ...))))))
                   (let ((if #t))
                     (when if (set! if 'now))
                     if)))
  (eq? result 'now))

(let ()
  (define-syntax foo
    (syntax-rules ()
      ((_ (a . (b . (c ...))) ...)
       '(foo (a c ... ) ...))))
  (equal? (foo (1 2 3 4 5) (6 7 8 9 10)) '(foo (1 3 4 5) (6 8 9 10))))

(let-syntax ((foo (syntax-rules ELLIPSIS ()
                    ((_ x ELLIPSIS #(y ELLIPSIS))
                     #(x ELLIPSIS y ELLIPSIS)))))
  (equal? #(1 2 3 4 5 6)
          (foo 1 2 3 #(4 5 6))))


(let ()
  (define-syntax foo
    (syntax-rules (abc xyz)
      ((_ abc x ...) (list 1000 x ...))
      ((_ xyz x ...) (list 2000 x ...))))
  (equal? '(2000 1 2 3)
          (foo xyz 1 2 3)))

(let ()
  (define-syntax foo
    (syntax-rules ()
      ((_ x ... "foo") (list 1000 x ...))
      ((_ x ... 3) (list 2000 x ...))))
  (equal? '(2000 1 2)
          (foo 1 2 3)))

(let ()
  (define-syntax foo
    (syntax-rules ()
      ((_ x . y)
       '("X" x "Y" y))))
  (equal? '("X" 1 "Y" (2 3 4))
          (foo 1 2 3 4)))

(let ()
  (define-syntax foo
    (syntax-rules ()
      ((_ x . y)
       '("X" x "Y" . y))))
  (equal? '("X" 1 "Y" 2 3 4)
          (foo 1 2 3 4)))

(let ()
  (define-syntax foo
    (syntax-rules ()
      ((_ x . y)
       '("X" x "Y" y))))
  (equal? '("X" 1 "Y" (2 3 . 4))
          (foo 1 2 3 . 4)))

(let ()
  (define-syntax foo
    (syntax-rules ()
      ((_ x . y)
       '("X" x "Y" . y))))
  (equal? '("X" 1 "Y" 2 3 . 4)
          (foo 1 2 3 . 4)))

(let ()
  (define-syntax foo
    (syntax-rules ()
      ((_ x . y)
       '("X" x "Y" y))))
  (equal? '("X" 1 "Y" 2)
          (foo 1 . 2)))

(let ()
  (define-syntax foo
    (syntax-rules ()
      ((_ x . y)
       '("X" x "Y" . y))))
  (equal? '("X" 1 "Y" . 2)
          (foo 1 . 2)))

(let ()
  ;; referring to a symbol (x) that will be defined later
  (define-syntax m
      (syntax-rules ()
        ((_) x)))
  (define (foo) (m))
  (define x 100)

  (= (foo) 100))

(let ()
  (define-syntax foo
    (syntax-rules ()
      ((_ "A" (x y) ...)
       '(AAA (x y) ...))
      ((_ "B" (x y) ...)
       '(BBB (x y ...) ...))))
  (and (equal? '(AAA (a 10) (b 20) (c 30))
               (foo "A" (a 10) (b 20) (c 30)))
       (equal? '(BBB (a 10 20 30) (b 10 20 30) (c 10 20 30))
               (foo "B" (a 10) (b 20) (c 30)))))

(let ()
  (define-syntax bar
    (syntax-rules ()
      ((_ (x y z ...) ...)
       '(AA (100 200 z ...) ...))))
  (equal? '(AA (100 200 3) (100 200) (100 200 c))
          (bar (1 2 3) (10 20) (a b c))))

(let ()
  (define-syntax foo
    (syntax-rules ()
      ((_ (x y ... z) ...)
       '(AA (100 x (y ...) z) ...))))
  (equal? '(AA (100 1 (2) 3) (100 a () b) (100 v (w x y) z))
          (foo (1 2 3) (a b) (v w x y z))))

(let ()
  (define-syntax foo
    (syntax-rules (xx)
      ((_ xx (x ...) ...)
       '(XX #(x) ... ...))))
  (equal? (foo xx (1 2 3) (a b))
          '(XX #(1) #(2) #(3) #(a) #(b))))

;; from: https://www.scheme.com/tspl4/further.html
(let ()
  (define in-range?
    (lambda (x n y)
      (and (>= n x) (< n y))))

  (define-syntax range-case
    (syntax-rules (- else)
      [(_ expr ((x - y) e1 e2 ...) ... [else ee1 ee2 ...])
       (let ([tmp expr])
         (cond
          [(in-range? x tmp y) e1 e2 ...]
          ...
          [else ee1 ee2 ...]))]
      [(_ expr ((x - y) e1 e2 ...) ...)
       (let ([tmp expr])
         (cond
          [(in-range? x tmp y) e1 e2 ...]
          ...))]))

  (define gpa->grade
    (lambda (x)
      (range-case x
        [(0 - 5) 'f]
        [(5 - 15) 'd]
        [(15 - 25) 'c]
        [(25 - 35) 'b]
        [else 'a])))

  (and (eq? 'f (gpa->grade 3))
       (eq? 'd (gpa->grade 10))
       (eq? 'c (gpa->grade 22))
       (eq? 'b (gpa->grade 34))
       (eq? 'a (gpa->grade 38))))

(let ()
  (define-syntax foo
    (syntax-rules ()
      ((_ x ...) '(x ... . 10))))

  (and (equal? '(1 2 . 10) (foo 1 2))
       (equal? 10 (foo))))

;; http://petrofsky.org/src/primer.txt
;; (use wayback machine)
(let ((a 1))
  (letrec-syntax
      ((foo (syntax-rules ()
              ((_ b)
               (bar a b))))
       (bar (syntax-rules ()
              ((_ c d)
               (cons c (let ((c 3))
                         (list d c 'c)))))))
    (let ((a 2))
      (equal? '(1 2 3 a)
              (foo a)))))

;; http://petrofsky.org/src/primer.txt
;; (use wayback machine)
(let ((x 1))
  (let-syntax
      ((foo (syntax-rules ()
              ((_ y) (let-syntax
                         ((bar (syntax-rules ()
                                 ((_) (let ((x 2)) y)))))
                       (bar))))))
    (= 1 (foo x))))

;; numbers

(integer? 1)
(rational? 1)
(real? 1)
(complex? 1)
(number? 1)

(not (integer? 1/3))
(rational? 1/3)
(real? 1/3)
(complex? 1/3)
(number? 1/3)

(not (integer? 1.0))
(not (rational? 1.0))
(real? 1.0)
(complex? 1.0)
(number? 1.0)

(not (integer? 2+3i))
(not (rational? 2+3i))
(not (real? 2+3i))
(complex? 2+3i)
(number? 2+3i)

;; section 6.2.6
(real? +inf.0)
(real? -inf.0)
(real? +nan.0)
(complex? +inf.0)
(complex? -inf.0)
(complex? +nan.0)
(number? +inf.0)
(number? -inf.0)
(number? +nan.0)

(not (= +nan.0 +nan.0))
(not (eqv? +nan.0 +nan.0))
(= +inf.0 +inf.0)
(eqv? +inf.0 +inf.0)
(= -inf.0 -inf.0)
(eqv? -inf.0 -inf.0)

(exact? 1)
(not (inexact? 1))

(exact? 1/3)
(not (inexact? 1/3))

(inexact? 1.0)
(not (exact? 1.0))

(exact? 1+2i)
(exact? 1/2+2i)
(exact? 1+2/3i)
(exact? 1/3+2/5i)
(not (inexact? 1+2i))
(not (inexact? 1/2+2i))
(not (inexact? 1+2/3i))
(not (inexact? 1/3+2/5i))

(inexact? 0.5+0.4i)
(inexact? 5+0.4i)
(inexact? 0.5+4i)
(inexact? 1/2+0.4i)
(inexact? 2.0+1/2i)
(not (exact? 0.5+0.4i))
(not (exact? 5+0.4i))
(not (exact? 0.5+4i))
(not (exact? 1/2+0.4i))
(not (exact? 2.0+1/2i))

(exact-integer? 4)
(exact-integer? -1)
(exact-integer? 0)
(exact-integer? 5+0i)
(exact-integer? 4/2)
(not (exact-integer? 4/3))
(not (exact-integer? 4.0))
(not (exact-integer? +i))

(= 9/4 2.25)
(not (eqv? 9/4 2.25))
(eqv? 9/4 (exact 2.25))

(= 2.0 4/2)
(not (eqv? 2.0 4/2))
(eqv? 2.0 (inexact 4/2))

(= 2.25i 9/4i)
(not (eqv? 2.25i 9/4i))
(= 1/3+2.25i 1/3+9/4i)
(not (eqv? 1/3+2.25i 1/3+9/4i))

(= 0 (numerator 0))
(= 1 (denominator 0))
(= 5 (numerator 5))
(= 1 (denominator 5))
(= 3 (numerator 6/14))
(= 7 (denominator 6/14))

(= 1 (real-part 1+i))
(= 1 (imag-part 1+i))
(= 1 (real-part 1-i))
(= -1 (imag-part 1-i))
(= 0 (real-part -i))
(= -1 (imag-part -i))
(= 0 (real-part +i))
(= 1 (imag-part +i))
(= 1 (real-part 1+inf.0i))
(= +inf.0 (imag-part 1+inf.0i))

(= 3-4i (make-rectangular 3 -4))
(= 3 (make-rectangular 3 0))
(= -1/3+5/8i (make-rectangular -1/3 5/8))

(= 1@2 (make-polar 1 2))

(= 1 (magnitude 1@-2))
(= -2 (angle 1@-2))

(> +inf.0 1)
(>= +inf.0 1)
(< -inf.0 -1)
(<= -inf.0 -1)
(> 4/5 2/3)
(>= 4/5 2/3)
(< -4/5 2/3)
(<= -4/5 2/3)

(= -inf.0 -inf.0)
(= +inf.0 +inf.0)
(not (= -inf.0 +inf.0))
(= -inf.0+i -inf.0+i)
(= +inf.0+i +inf.0+i)
(= 1+inf.0i 1+inf.0i)
(= 1-inf.0i 1-inf.0i)

(not (> +nan.0 0))
(not (< +nan.0 0))
(not (>= +nan.0 0))
(not (<= +nan.0 0))
(not (= +nan.0 0))

;; inexact numbers and exact numbers are never eqv (section 6.1), so they can't
;; be eq either.
(not (eqv? #i1 1))
(not (eq? #i1 1))
(= #i1 1)
(equal? #i1 1)

(eqv? 1. #i1)
(= 1. #i1)
(equal? 1. #i1)

(eqv? 1 2/2)
(= 1 2/2)
(equal? 1 2/2)

(eqv? 1+1i 1+2/2i)
(= 1+1i 1+2/2i)
(equal? 1+1i 1+2/2i)

(zero? 0)
(zero? 0.0)
(zero? 0/4)
(zero? 0+0i)

(eqv? 9/4 (exact 2.25))
(eqv? 29/8 (exact 3.625))
(eqv? 2.25 (inexact 9/4))
(eqv? 3.625 (inexact 29/8))

(= 16 #x10)
(= 10 #d10)
(=  8 #o10)
(=  2 #b10)

(= 16 #e#x10)
(= 10 #e#d10)
(=  8 #e#o10)
(=  2 #e#b10)

(= 16. #i#x10)
(= 10. #i#d10)
(=  8. #i#o10)
(=  2. #i#b10)

(= 1.0 #i1)
(= 1 #e1.0)

(= 1/16 #x1/10)
(= 1/10 #d1/10)
(= 1/8  #o1/10)
(= 1/2  #b1/10)

(= 17+16i #x11+10i)
(= 11+10i #d11+10i)
(= 9+8i #o11+10i)
(= 3+2i #b11+10i)

(= 1/5 (/ 5))

(= +inf.0 (+ +inf.0 10))
(= +inf.0 (* +inf.0 2))
(= +inf.0 (- +inf.0 5))
(= +inf.0 (/ +inf.0 2))
(= -inf.0 (- +inf.0))

(= -inf.0 (+ -inf.0 10))
(= -inf.0 (* -inf.0 2))
(= -inf.0 (- -inf.0 5))
(= -inf.0 (/ -inf.0 2))
(= +inf.0 (- -inf.0))

(= +inf.0 (/ 1 0.0))
(= -inf.0 (/ 1 -0.0))
(= -inf.0 (/ -1 0.0))
(= +inf.0 (/ -1 -0.0))

(= +inf.0+inf.0i (/ 2+3i 0.0))
(= -inf.0-inf.0i (/ 2+3i -0.0))
(= -inf.0+inf.0i (/ -2+3i 0.0))
(= +inf.0-inf.0i (/ 2-3i 0.0))
(= +inf.0-inf.0i (/ -2+3i -0.0))
(= -inf.0+inf.0i (/ 2-3i -0.0))
(nan? (/ 0 0.0))
(nan? (/ 0.0 0.0))
(= +inf.0 (/ +inf.0 0.0))
(= -inf.0 (/ -inf.0 0.0))
(nan? (/ +nan.0 0.0))
(nan? (/ +nan.0 2))
(nan? (/ +nan.0 +inf.0))
(nan? (/ +nan.0 -inf.0))

(= 6+8i (+ 2+3i 4+5i))
(= -2-2i (- 2+3i 4+5i))
(= -7+22i (* 2+3i 4+5i))
(= 23/41+2/41i (/ 2+3i 4+5i))

;; just make sure these literals don't cause an error
(begin 1+i #t)
(begin 1-i #t)
(begin +i #t)
(begin -i #t)
(begin 1+inf.0i #t)
(begin 1-inf.0i #t)
(begin 1+nan.0i #t)
(begin 1-nan.0i #t)
(begin +inf.0i #t)
(begin -inf.0i #t)
(begin +nan.0i #t)
(begin -nan.0i #t)
(begin #x+inf.0 #t)
(begin #x-inf.0 #t)
(begin #x+nan.0 #t)
(begin #x-nan.0 #t)
(begin #o+inf.0 #t)
(begin #o-inf.0 #t)
(begin #o+nan.0 #t)
(begin #o-nan.0 #t)
(begin #d+inf.0 #t)
(begin #d-inf.0 #t)
(begin #d+nan.0 #t)
(begin #d-nan.0 #t)
(begin #b+inf.0 #t)
(begin #b-inf.0 #t)
(begin #b+nan.0 #t)
(begin #b-nan.0 #t)
(begin #i#x+inf.0 #t)
(begin #i#x-inf.0 #t)
(begin #i#x+nan.0 #t)
(begin #i#x-nan.0 #t)
(begin #i#o+inf.0 #t)
(begin #i#o-inf.0 #t)
(begin #i#o+nan.0 #t)
(begin #i#o-nan.0 #t)
(begin #i#d+inf.0 #t)
(begin #i#d-inf.0 #t)
(begin #i#d+nan.0 #t)
(begin #i#d-nan.0 #t)
(begin #i#b+inf.0 #t)
(begin #i#b-inf.0 #t)
(begin #i#b+nan.0 #t)
(begin #i#b-nan.0 #t)
(begin #i+inf.0 #t)
(begin #i-inf.0 #t)
(begin #i+nan.0 #t)
(begin #i-nan.0 #t)

;; math

(nan? +nan.0)
(nan? +nan.0i)
(nan? -nan.0)
(nan? -nan.0i)
(nan? +nan.0+nan.0i)
(nan? -nan.0+nan.0i)
(nan? +nan.0-nan.0i)
(nan? -nan.0-nan.0i)
(not (nan? +inf.0))
(not (nan? -inf.0))
(not (nan? +inf.0i))
(not (nan? -inf.0i))
(not (nan? 0))
(not (nan? 1.0))
(not (nan? +i))
(not (nan? 1+2i))

(nan? (- +inf.0 +inf.0))
(nan? (- -inf.0 -inf.0))

(= +inf.0 (+ +inf.0 +inf.0))
(= -inf.0 (+ -inf.0 -inf.0))
(= +inf.0 (- +inf.0 -inf.0))
(= -inf.0 (- -inf.0 +inf.0))

(eqv? -0.0 -0.0)
(not (eqv? 0.0 -0.0))
(eqv? -0.0 (- 0.0))
(eqv? 0.0 (- -0.0))
(eqv? -0.0 (+ -0.0 -0.0))
(eqv? -0.0 (- -0.0 0.0))
(eqv? -0.0 (- -0.0 0))
(eqv? 0.0 (abs -0.0))

(finite? 0.0)
(finite? -0.0)
(finite? 1.0)
(finite? -1.0)
(finite? +i)
(finite? -i)
(finite? 1+2i)
(finite? -1-3i)
(not (finite? +inf.0))
(not (finite? -inf.0))
(not (finite? +nan.0))
(not (finite? -nan.0))
(not (finite? +inf.0+i))
(not (finite? -inf.0-2i))
(not (finite? +nan.0+3i))
(not (finite? -nan.0-4i))
(not (finite? +inf.0i))
(not (finite? -inf.0i))
(not (finite? +nan.0i))
(not (finite? -nan.0i))
(not (finite? 1+inf.0i))
(not (finite? 2-inf.0i))
(not (finite? 3+nan.0i))
(not (finite? 4-nan.0i))
(not (finite? +nan.0i))
(not (finite? -nan.0i))
(not (finite? +nan.0+i))
(not (finite? -nan.0-2i))

(infinite? +inf.0)
(infinite? -inf.0)
(infinite? +inf.0i)
(infinite? -inf.0i)
(infinite? 1+inf.0i)
(infinite? 2-inf.0i)
(infinite? 3+inf.0i)
(infinite? 4-inf.0i)
(not (infinite? 0))
(not (infinite? 0.0))
(not (infinite? -0.0))
(not (infinite? 1))
(not (infinite? -4.2))
(not (infinite? +nan.0))
(not (infinite? -nan.0))
(not (infinite? +nan.0i))
(not (infinite? -nan.0i))
(not (infinite? 1+nan.0i))
(not (infinite? -1-nan.0i))

;; multiplying infinity with exact zero should be zero
(= 0 (* 0 +inf.0))
(= 0 (* 0 -inf.0))
(= 0 (* 0 +inf.0i))
(= 0 (* 0 -inf.0i))

;; multiplying infinity with inexact zero should be nan
(nan? (* 0.0 +inf.0))
(nan? (* 0.0 -inf.0))
(nan? (* 0.0 +inf.0i))
(nan? (* 0.0 -inf.0i))

(= -0.0 (sin -0.0))
(= 0.0 (sin 0.0))
(approx= 1.0 (sin (/ pi 2)))
(approx= 0.0 (sin pi))
(approx= -1.0 (sin (* 3/2 pi)))

(= -0.0 (asin -0.0))
(= 0.0 (asin 0.0))
(approx= (/ pi 2) (asin 1.0))
(approx= 0.0 (asin 0.0))
(approx= (* -1/2 pi) (asin -1))

(eqv? 1.0 (cos 0.0))
(eqv? 1.0 (cos -0.0))
(approx= 0.0 (cos (/ pi 2)))
(approx= -1.0 (cos pi))

(eqv? 0.0 (acos 1.0))
(approx= (/ pi 2) (acos 0.0))
(approx= pi (acos -1.0))

(eqv? 0.0 (tan 0.0))
(eqv? -0.0 (tan -0.0))

;; 6.2.6 under "log"
(= -inf.0 (log 0.0))
(approx= (+ -inf.0 (* pi 1i)) (log -0.0))

;; from section 6.2.4
(approx= (- pi) (imag-part (log -1.0-0.0i)))

;; we use eqv? in the following comparisons, instead of =, because exactness
;; matters.
(eqv? -5.0 (floor -4.3))
(eqv? -4.0 (ceiling -4.3))
(eqv? -4.0 (truncate -4.3))
(eqv? 4.0 (floor 4.3))
(eqv? 5.0 (ceiling 4.3))
(eqv? 4.0 (truncate 4.3))

(eqv? -4.0 (round -4.3))
(eqv? 5.0 (round 4.7))

(eqv? 2 (floor 8/3))
(eqv? 3 (ceiling 8/3))
(eqv? 2 (truncate 8/3))

;; round towards even
(eqv? -4.0 (round -4.5))
(eqv? -6.0 (round -5.5))
(eqv? 4 (round 7/2))
(eqv? -4 (round -7/2))

(let-values (((s r) (exact-integer-sqrt 8)))
  (and (eqv? s 2)
       (eqv? r 4)))

(let-values (((s r) (exact-integer-sqrt 16)))
  (and (eqv? s 4)
       (eqv? r 0)))

;; using eqv? to make sure exactness matches between input and result. not sure
;; if this is required by the report, but some other implementations seem to
;; work like this.
(eqv? 3.0 (sqrt 9.0))
(eqv? 3 (sqrt 9))
(eqv? +i (sqrt -1))
(eqv? +1/3i (sqrt -1/9))
(eqv? +1.0i (sqrt -1.0))
(eqv? 1+i (sqrt 2i))
(eqv? 1/3+4/5i (sqrt -119/225+8/15i))
(approx= 8.0 (square (sqrt 8)))
(approx= -8.0 (square (sqrt -8)))
(eqv? 1.0 (sqrt 1.0))
(eqv? 1 (sqrt 1))

(eqv? 1/4 (rationalize 8/45 1/7))
(eqv? 1/5 (rationalize 10003/99482 1/10))
(eqv? 1/9 (rationalize 10003/99482 1/50))
(eqv? 1/3 (rationalize (exact 0.3) 1/10))
(eqv? #i1/3 (rationalize 0.3 1/10))
(eqv? -1.8 (rationalize -1.871 1/10))
(zero? (rationalize 101 +inf.0))
(zero? (rationalize 101 -inf.0))
(nan? (rationalize +nan.0 1))
(nan? (rationalize 1 +nan.0))

(eqv? 0 (expt 0 3))
(eqv? 0.0 (expt 0.0 3))
(eqv? 1 (expt 1 3))
(eqv? 81 (expt 3 4))
(eqv? 1/81 (expt 3 -4))
(approx= #i1/3 (expt 1/9 1/2))
(approx= -0.7530458367485594-0.9864287886477446i (expt 2+3i 4+5i))
(approx= (sqrt 3) (expt 3 1/2))
(zero? (imag-part (expt +i +i)))

(equal? "143.1" (number->string 143.1))
(equal? "3e8" (number->string 1000 16))
(equal? "1750" (number->string 1000 8))
(equal? "1111101000" (number->string 1000 2))
