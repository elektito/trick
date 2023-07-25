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

(eq? 1 1)
(eq? 'foo 'foo)
(eq? "foo" "foo")
(eq? :foo :foo)

(eq? #f '#f)
(eq? #t '#t)
(eq? 10 '10)
(eq? "foo" '"foo")
(eq? #\space #\space)
(eq? '() '())
(not (eq? '() 'nil))

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

(let ((x 100))
  (set! x 200)
  (eq? 200 x))

(let ((x 100))
  (define (foo x) (+ 900 x))
  (eq? 1000 (foo 100)))

(let ()
  (define foo)
  (eq? foo '()))

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
(eq? 100 (if :foo 100 200))
(eq? 100 (if "foo" 100 200))
(eq? 100 (if (lambda (x) x) 100 200))
(eq? 100 (if #t 100))
(eq? '() (if #f 100))

(eq? 3 (when #t 1 2 3))
(eq? #f (when #f 1 2 3))
(eq? #f (unless #t 1 2 3))
(eq? 3 (unless #f 1 2 3))

(cond (#t #t))
(= 200 (cond ((= 10 20) 50 100)
             ((= 5 5) 80 200)
             ((= 40 40) 300)))

;; regression test: putting one-armed if in a lambda just to make sure the
;; "join" instruction is correctly generated for the implied "false" branch.
(eq? '() ((lambda () (if #f 100))))

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
(eq? 0 (/ 5))
(eq? 2 (/ 10 5))
(eq? 20 (/ 3000 10 5 3))

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

(eq? 'nil (type '()))
(eq? 'symbol (type :foo))
(eq? 'pair (type '(1 2)))
(eq? 'int (type 42))
(eq? 'string (type "foo"))
(eq? 'char (type #\space))
(eq? 'closure (type (lambda (x) x)))
(eq? 'bool (type #f))
(eq? 'bool (type #t))

(null? '())
(not (null? '(1)))
(not (null? '(1 2)))
(not (null? '(1 . 2)))
(not (null? 1))
(not (null? "foo"))
(not (null? 'foo))
(not (null? (lambda (x) x)))

(pair? '(1))
(pair? '(1 2))
(pair? '(1 2 3))
(pair? '(1 . 2))
(not (pair? '()))
(not (pair? 'foo))
(not (pair? "foo"))
(not (pair? 1))
(not (pair? (lambda (x) x)))

(list? '())
(list? '(1))
(list? '(1 2))
(not (list? '(1 . 2)))
(not (list? '(1 2 . 3)))
(not (list? (lambda (x) x)))

(symbol? 'foo)
(symbol? :foo)
(not (symbol? '()))
(not (symbol? 1))
(not (symbol? "foo"))
(not (symbol? '(1 . 2)))
(not (symbol? '(1 2)))
(not (symbol? (lambda (x) x)))

(integer? -1)
(integer? 0)
(integer? 1)
(integer? #x10)
(not (integer? 'foo))
(not (integer? "foo"))
(not (integer? '()))
(not (integer? '(1)))
(not (integer? '(1 2)))
(not (integer? '(1 . 2)))
(not (integer? (lambda (x) x)))

(string? "")
(string? "foo")
(not (string? 'foo))
(not (string? 1))
(not (string? '()))
(not (string? '(1)))
(not (string? '(1 2)))
(not (string? '(1 . 2)))
(not (string? (lambda (x) x)))

(closure? (lambda (x) x))
(closure? (lambda () 10))
(not (closure? 'foo))
(not (closure? 1))
(not (closure? "foo"))
(not (closure? '()))
(not (closure? '(1)))
(not (closure? '(1 2)))
(not (closure? '(1 . 2)))

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

;; call/cc tests
;;
;; some tests adapted from chibi scheme test suite. see:
;; https://github.com/ashinn/chibi-scheme/blob/master/tests/r5rs-tests.scm
;; https://github.com/ashinn/chibi-scheme/blob/master/tests/r7rs-tests.scm

(eq? (call/cc type) 'closure)
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

(closure? cons)
(equal? '(1 2) (apply cons '(1 (2))))

(closure? car)
(equal? 1 (apply car '((1 2))))

(closure? cdr)
(equal? '(2) (apply cdr '((1 2))))

(closure? #$iadd)
(equal? 3 (apply #$iadd '(1 2)))

(closure? #$isub)
(equal? -1 (apply #$isub '(1 2)))

(closure? #$imul)
(equal? 2 (apply #$imul '(1 2)))

(closure? #$idiv)
(equal? 2 (apply #$idiv '(4 2)))

(closure? #$shr)
(equal? 1 (apply #$shr '(4 2)))

(closure? #$shl)
(equal? 16 (apply #$shl '(4 2)))

(closure? #$asr)
(equal? 1 (apply #$asr '(4 2)))

(closure? #$bnot)
(equal? -1 (apply #$bnot '(0)))

(closure? #$band)
(equal? 0 (apply #$band '(0 1)))

(closure? #$bor)
(equal? 1 (apply #$bor '(0 1)))

(closure? #$bxor)
(equal? 1 (apply #$bxor '(0 1)))

(closure? #$ilt)
(apply #$ilt '(1 2))

(closure? #$ile)
(apply #$ile '(1 1))

(closure? type)
(equal? 'symbol (apply type '(x)))

(closure? eq?)
(apply eq? '(foo foo))

(closure? gensym)
(symbol? (apply gensym))

(closure? char->integer)
(equal? 65 (apply char->integer '(#\A)))

(closure? integer->char)
(equal? #\A (apply integer->char '(65)))

(closure? char-upcase)
(equal? #\A (apply char-upcase '(#\a)))

(closure? char-downcase)
(equal? #\a (apply char-downcase '(#\A)))

(closure? char-foldcase)
(equal? #\a (apply char-foldcase '(#\A)))

(closure? digit-value)
(equal? 1 (apply digit-value '(#\1)))

(closure? make-string)
(equal? "AAA" (apply make-string '(3 #\A)))
(equal? 3 (string-length (apply make-string '(3))))

(closure? string-ref)
(equal? #\C (string-ref "ABCD" 2))

(closure? string-set!)
(let ((x (make-string 4 #\A)))
  (apply string-set! `(,x 2 #\X))
  (equal? "AAXA" x))

(closure? string-length)
(equal? 4 (apply string-length '("AAAA")))

(closure? call/cc)
(apply call/cc (list (lambda (k) (k #t))))

(closure? call-with-current-continuation)
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
