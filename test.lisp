(eq? 1 1)
(eq? 'foo 'foo)
(eq? "foo" "foo")
(eq? :foo :foo)

(eq? #f '#f)
(eq? #t '#t)
(eq? 10 '10)
(eq? "foo" '"foo")
(eq? '() '())
(not (eq? '() 'nil))

(let ((x 100))
  (set! x 200)
  (eq? 200 x))

(let ((x 100))
  (define (foo x) (+ 900 x))
  (eq? 1000 (foo 100)))

(let ()
  (define foo)
  (eq? foo '()))

(eq? 100 (if #t 100 200))
(eq? 200 (if #f 100 200))
(eq? 100 (if 0 100 200))
(eq? 100 (if 1 100 200))
(eq? 100 (if :foo 100 200))
(eq? 100 (if "foo" 100 200))
(eq? 100 (if (lambda (x) x) 100 200))
(eq? 100 (if #t 100))
(eq? '() (if #f 100))

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

(eq? 7 (shr 29 2))
(eq? 116 (shl 29 2))
(eq? 7 (asr 29 2))
(eq? -8 (asr -29 2))

(eq? -1 (b-not 0))
(eq? 0 (b-not -1))
(eq? -2 (b-not 1))

(eq? 0 (b-and 0 0))
(eq? 0 (b-and 0 1))
(eq? 0 (b-and 1 0))
(eq? 1 (b-and 1 1))
(eq? #xaa00 (b-and #xaabb #xff00))
(eq? 0 (b-and 19999 0))
(eq? 19999 (b-and 19999 -1))

(eq? 0 (b-or 0 0))
(eq? 1 (b-or 0 1))
(eq? 1 (b-or 1 0))
(eq? 1 (b-or 1 1))
(eq? #xffbb (b-or #xaabb #xff00))
(eq? 19999 (b-or 19999 0))

(eq? 0 (b-xor 0 0))
(eq? 1 (b-xor 0 1))
(eq? 1 (b-xor 1 0))
(eq? 0 (b-xor 1 1))
(eq? 5 (b-xor #xf #xa))

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
                           (is-odd? (isub n 1)))))
         (is-odd? (lambda (n)
                    (and (not (eq? n 0))
                         (is-even? (isub n 1))))))
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

(eq? 'nil (type '()))
(eq? 'symbol (type :foo))
(eq? 'pair (type '(1 2)))
(eq? 'int (type 42))
(eq? 'string (type "foo"))
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

(eq? 0 (length '()))
(eq? 1 (length '(1)))
(eq? 2 (length '(1 2)))
(eq? 3 (length '(1 2 3)))

(not (eq? (gensym) (gensym)))
(let ((gs (gensym)))
  (eq? gs gs))

(list-eq? '(1 foo 2) '(1 foo 2))
(not (list-eq? '(1 foo 2) '(1 foo 2 3)))
(list-eq? '(1 (foo) 2) '(1 (foo) 2))
(list-eq? '(1 (foo "bar") 2) '(1 (foo "bar") 2))
(not (list-eq? '(1 (foo "bar") 2) '(1 (foo "bar" 10) 2)))

(list-eq? '(11 20 110)
          (mapcar (lambda (n) (+ n 10)) '(1 10 100)))
(list-eq? '(10 100 1000)
          (map + '(1 10 100) '(2 20 200) '(3 30 300) '(4 40 400)))

(= 1)
(= 1 1)
(= 1 1 1)
(not (= 1 2))
(not (= 1 1 2))
(= '(1 foo 2) '(1 foo 2))
(not (= '(1 foo 2) '(1 foo 2 3)))
(= '(1 (foo) 2) '(1 (foo) 2))
(= '(1 (foo "bar") 2) '(1 (foo "bar") 2))
(not (= '(1 (foo "bar") 2) '(1 (foo "bar" 10) 2)))

(eq? 6 (apply + '(1 2 3)))
(eq? 10 (apply + '(1 2 3 4)))
(eq? 0 (apply + '()))
(= '(1 2 3 foo bar)
   (apply list 1 2 3 '(foo bar)))

;; backquote tests

(eq? `() '())
(eq? `a 'a)
(eq? `,10 10)
(let ((x '(3 4)))
  (list-eq? `(1 2 ,@x 5)
            '(1 2 3 4 5)))
(let ((x '(3 4)))
  (= `(1 2 ((,@x)) 5)
     '(1 2 ((3 4)) 5)))
(let ((x 10) (y 20) (z 30))
  (= `(x ,y z) '(x 20 z)))
(let ((x 10) (y 20) (z 30))
  (= `(,x ,y ,z) '(10 20 30)))
(let ((x 10) (y 20) (z 30))
  (= `(x y z) '(x y z)))
(let ((x 10) (y 20) (z 30))
  (= `(x ((,y)) z) '(x ((20)) z)))
(let ((x 10) (y 20) (z 30))
  (= ```(x ,,,y z) '``(x ,,20 z)))
(= ``(a ,,(+ 1 2) ,(+ 2 3))
   '`(a ,3 ,(+ 2 3)))
(= ``,,3 '`,3)
(= ```,,,3 '``,,3)

;; the following are adopted from husk scheme test suite. see
;; https://github.com/justinethier/husk-scheme/blob/master/tests/t-backquote.scm
(= `(list ,(car '(3 6)) 4)
   '(list 3 4))
(= (let ((name 'a)) `(list ,name ',name))
   '(list a (quote a)))
(= (let ((name 'a)) '(list ,name ',name))
   '(list (unquote name) (quote (unquote name))))
(= (let ((name 'a)) `(list ,name (,name)))
   '(list a (a)))
(= (let ((name 'a)) `(list ,name ((,name))))
   '(list a ((a))))
(= `(a `(b ,(car '(3 6)) ,(foo ,(car '(3 6)) d) e) f)
   '(a `(b ,(car '(3 6)) ,(foo 3 d) e) f))
(= (let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,,name2 d) e))
   '(a `(b ,x ,y d) e))
(= (let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))
   '(a `(b ,x ,'y d) e))
(= (backquote (list (unquote (car '(3 6))) 4))
   '(list 3 4))
(= '(backquote (list (unquote (car '(3 6))) 4))
   '`(list ,(car '(3 6)) 4))
(= `(a `(b ,(foo ,(car '(3 6))) c) d)
   '(a `(b ,(foo 3) c) d))

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
