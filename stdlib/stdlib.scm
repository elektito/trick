;; macros

(define-syntax and
  (syntax-rules ()
    ((_) #t)
    ((_ x) x)
    ((_ x y ...) (if x (and y ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((_) #f)
    ((_ x) x)
    ((_ x y ...) (let ((tmp x))
                   (if tmp tmp (or y ...))))))

(define-syntax when
  (syntax-rules ()
    ((_ condition body1 body2 ...)
     (if condition (begin body1 body2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ condition body1 body2 ...)
     (if condition (#$void) (begin body1 body2 ...)))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...)
       body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
         body1 body2 ...)))))

(define-syntax let/cc
  (syntax-rules ()
    ((_ k body1 body2 ...)
     (call/cc (lambda (k) body1 body2 ...)))))

(define-syntax case-lambda
  (syntax-rules ()
    ((_ (params body0 ...) ...)
     (lambda args
       (let ((len (length args)))
         (letrec-syntax
             ((cl (syntax-rules ::: ()
                    ((cl)
                     (error "no matching clause" args))
                    ((cl ((p :::) . body) . rest)
                     (if (= len (length '(p :::)))
                         (apply (lambda (p :::)
                                  . body)
                                args)
                         (cl . rest)))
                    ((cl ((p ::: . tail) . body)
                         . rest)
                     (if (>= len (length '(p :::)))
                         (apply
                          (lambda (p ::: . tail)
                            . body)
                          args)
                         (cl . rest)))
                    ((cl (p . body) . rest)
                     (apply (lambda p . body) args)))))
           (cl (params body0 ...) ...)))))))

(define-syntax do-step
  (syntax-rules ()
    ((_ x) x)
    ((_ x y) y)))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
       command ...)
     (letrec
         ((loop
           (lambda (var ...)
             (if test
                 (begin
                   (#$void)
                   expr ...)
                 (begin
                   command
                   ...
                   (loop (do-step var step ...)
                         ...))))))
       (loop init ...)))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values (binding ...) body0 body1 ...)
     (let-values "bind"
       (binding ...) () (begin body0 body1 ...)))
    ((let-values "bind" () tmps body)
     (let tmps body))
    ((let-values "bind" ((b0 e0)
                         binding ...) tmps body)
     (let-values "mktmp" b0 e0 ()
                 (binding ...) tmps body))
    ((let-values "mktmp" () e0 args
                 bindings tmps body)
     (call-with-values
         (lambda () e0)
       (lambda args
         (let-values "bind"
           bindings tmps body))))
    ((let-values "mktmp" (a . b) e0 (arg ...)
                 bindings (tmp ...) body)
     (let-values "mktmp" b e0 (arg ... x)
                 bindings (tmp ... (a x)) body))
    ((let-values "mktmp" a e0 (arg ...)
                 bindings (tmp ...) body)
     (call-with-values
         (lambda () e0)
       (lambda (arg ... . x)
         (let-values "bind"
           bindings (tmp ... (a x)) body))))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body0 body1 ...)
     (let () body0 body1 ...))
    ((let*-values (binding0 binding1 ...)
       body0 body1 ...)
     (let-values (binding0)
       (let*-values (binding1 ...)
         body0 body1 ...)))))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ("step")
       ((param value p old new) ...)
       ()
       body)
     (let ((p param) ...)
       (let ((old (p)) ...
             (new value) ...)
         (dynamic-wind
             (lambda () (p new) ...)
             (lambda () . body)
             (lambda () (p old #t) ...)))))
    ((parameterize ("step")
       args
       ((param value) . rest)
       body)
     (parameterize ("step")
       ((param value p old new) . args)
       rest
       body))
    ((parameterize ((param value) ...) . body)
     (parameterize ("step")
       ()
       ((param value) ...)
       body))))

(define-syntax with-gensyms
  (syntax-rules ()
    ((_ (name1 name2 ...) b1 b2 ...)
     (with-gensyms "gen-bindings" (name1 name2 ...) (b1 b2 ...)))

    ((_ "with-bindings" bindings . body)
     (let bindings . body))

    ((_ "gen-bindings" (name ...) (b1 b2 ...))
     (with-gensyms "with-bindings" ((name (gensym (symbol->string 'name))) ...)
                   b1 b2 ...))))

(define-syntax case
  (syntax-rules (else =>)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else => result))
     (result key))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) => result))
     (if (memv key '(atoms ...))
         (result key)))
    ((case key
       ((atoms ...) => result)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (result key)
         (case key clause clauses ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))

;; comparison

(define (eqv? x y)
  ;; for our current implementation, eq? already does the same thing as eqv?
  ;; should
  (eq? x y))

;; primcalls

(define (make-string . args)
  (if (null? (cdr args))
      (#$make-string (car args) #\null)
      (#$make-string (car args) (cadr args))))

(define (apply fn . args)
  ;; last argument must be a list
  (#$apply fn (append (butlast args) (last args))))

(define (gensym . x)
  (if (null? x)
      (#$gensym (#$void))
      (#$gensym (car x))))

;; type predicates

(define (null? x) (eq? 'nil (#$type x)))

(define (atom? v)
  ;; everything besides cons (3) is an atom
  (if (eq? (#$type v) 'pair) #f #t))

(define (symbol? v) (eq? (#$type v) 'symbol))
(define (pair? v) (eq? (#$type v) 'pair))
(define (string? v) (eq? (#$type v) 'string))
(define (char? v) (eq? (#$type v) 'char))
(define (procedure? v) (eq? (#$type v) 'procedure))
(define (boolean? v) (eq? (#$type v) 'bool))
(define (list? v)
  (if (null? v)
      #t
      (if (pair? v)
          (list? (cdr v))
          #f)))
(define (vector? v)
  (eq? (#$type v) 'vector))
(define (bytevector? v)
  (eq? (#$type v) 'bytevector))
(define (port? v)
  (eq? (#$type v) 'port))
(define (void? v)
  (eq? (#$type v) 'void))

(define (integer? v)
  (eq? (#$type v) 'int))
(define (rational? v)
  ;; "type" returns "rational" only for strictly rational numbers, so we need to
  ;; check for both rational and integer here.
  (or (eq? (#$type v) 'rational)
      (eq? (#$type v) 'int)))
(define (float? v)
  (eq? (#$type v) 'float))
(define (real? v)
  (or (eq? (#$type v) 'rational)
      (eq? (#$type v) 'int)
      (eq? (#$type v) 'float)))
(define (complex? v)
  (or (eq? (#$type v) 'complex)
      (eq? (#$type v) 'rational)
      (eq? (#$type v) 'int)
      (eq? (#$type v) 'float)))
(define (number? v)
  (complex? v))

;; booleans

(define (not x)
  (if x #f #t))

(define (boolean=? x y)
  (if x y (not y)))

;; symbols

(define (symbol=? x y)
  (eq? x y))

;; numbers

(define (< x y)
  (#$ilt x y))
(define (<= x y)
  (#$ile x y))
(define (> x y)
  (#$igt x y))
(define (>= x y)
  (#$ige x y))
(define (zero? x) (or (eqv? x 0)
                      (eqv? x 0.0)))
(define (negative? x) (< x 0))
(define (positive? x) (> x 0))
(define (even? n)
  (unless (integer? n)
    (error "not an integer"))
  (zero? (modulo n 2)))
(define (odd? n)
  (unless (integer? n)
    (error "not an integer"))
  (not (zero? (modulo n 2))))

(define (= . numbers)
  (define (cmp x y)
    (eqv? (inexact x) (inexact y)))
  (if (null? (cdr numbers))
      #t
      (all? (pairwise cmp numbers))))

(define (exact n)
  (if (float? n)
      (float->rational n)
      n))

(define (inexact n)
  (cond ((integer? n) (integer->float n))
        ((rational? n) (/ (integer->float (numerator n))
                          (integer->float (denominator n))))
        ((float? n) n)
        ((complex? n) (make-rectangular (inexact (real-part n))
                                        (inexact (imag-part n))))
        (else n)))

(define (exact? n)
  (not (inexact? n)))

(define (inexact? n)
  (if (eq? (type n) 'complex)
      (or (inexact? (real-part n))
          (inexact? (imag-part n)))
      (eq? (type n) 'float)))

(define (exact-integer? x)
  ;; all our integers are exact
  (integer? x))

(define (make-rectangular x y)
  (+ x (* y +i)))

(define (make-polar mag ang)
  (make-rectangular (* mag (cos ang))
                    (* mag (sin ang))))

(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

;; math

(define pi 3.141592653589793)
(define e 2.718281828459045)

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (exp z)
  (#$/math/exp z))

(define log
  (case-lambda
   ((z) (#$/math/ln z))
   ((z base) (#$/math/log z base))))

(define (sin z)
  (#$/math/sin z))

(define (cos z)
  (#$/math/cos z))

(define (tan z)
  (#$/math/tan z))

(define (asin z)
  (#$/math/asin z))

(define (acos z)
  (#$/math/acos z))

(define atan
  (case-lambda
   ((z) (#$/math/atan z))
   ;; the reports defines the value of (atan y x) as:
   ;; (angle (make-rectangular x y)))
   ((x y) (#$/math/atan2 x y))))

(define (sqrt z)
  (#$/math/sqrt z))

(define (square z)
  (* z z))

(define (exact-integer-sqrt k)
  (unless (and (integer? k) (not (negative? k)))
    (error "expected non-negative integer"))
  (let* ((s (exact (truncate (sqrt k))))
         (r (- k (square s))))
    (values s r)))

(define (floor x)
  (#$/math/floor x))

(define (ceiling x)
  (#$/math/ceiling x))

(define (truncate x)
  (#$/math/truncate x))

(define (round x)
  (#$/math/round x))

(define (nan? z)
  (#$/math/isnan z))

(define (infinite? z)
  (let ((z-real (real-part z))
        (z-imag (imag-part z)))
    (or (= +inf.0 z-real)
        (= -inf.0 z-real)
        (= +inf.0 z-imag)
        (= -inf.0 z-imag))))

(define (finite? z)
  (let ((z-real (real-part z))
        (z-imag (imag-part z)))
    (and (not (nan? z))
         (not (infinite? z)))))

;;

(define approx=
  (case-lambda
   ((x y) (approx= x y 0.0000001))
   ((x y epsilon) (let ((x-real (real-part x))
                        (y-real (real-part y))
                        (x-imag (imag-part x))
                        (y-imag (imag-part y)))
                    ;; we check both for actual equality and approximate
                    ;; equality. the reason for that is that approximate
                    ;; equality does not work for two infinities (since the
                    ;; difference between two infinities is a nan)
                    (or (and (= x-real y-real) (= x-imag y-imag))
                        (and (< (abs (- x-real y-real))
                                epsilon)
                             (< (abs (- x-imag y-imag))
                                epsilon)))))))

;; list utilities

(define (list . values) values)

(define (list* first . rest)
  (if (null? rest)
      first
      (cons first (apply list* rest))))

(define (append1 ls1 ls2)
  (if (null? ls1)
      ls2
      (if (null? ls2)
          ls1
          (cons (car ls1) (append1 (cdr ls1) ls2)))))

(define (append . lists)
  ;; notice that we have to use #$apply here, since apply itself depends on
  ;; "append"
  (if (null? lists)
      '()
      (append1 (car lists) (#$apply append (cdr lists)))))

(define (last x)
  (if (null? x)
      '()
      (if (null? (cdr x))
          (car x)
          (last (cdr x)))))

(define (butlast x)
  (if (null? x)
      '()
      (if (null? (cdr x))
          '()
          (cons (car x) (butlast (cdr x))))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (mapcar func args)
  (if (null? args)
      '()
      (cons (func (car args))
            (mapcar func (cdr args)))))

(define (reverse1 ls acc)
  (if (null? ls)
      acc
      (reverse1 (cdr ls) (cons (car ls) acc))))

(define (reverse ls)
  (reverse1 ls '()))

(define (any? values)
  (if (null? values)
      #f
      (if (car values)
          #t
          (any? (cdr values)))))

(define (all? values)
  (if (null? values)
      #t
      (if (car values)
          (all? (cdr values))
          #f)))

(define (map1 func arg-lists acc)
  (if (any? (mapcar null? arg-lists))
      (reverse acc)
      (map1 func
            (mapcar cdr arg-lists)
            (cons (apply func (mapcar car arg-lists)) acc))))

(define (map func . arg-lists)
  (map1 func arg-lists '()))

;; general comparison

(define (memq obj ls)
  (if (null? ls)
      #f
      (or (eq? obj (car ls))
          (memq obj (cdr ls)))))

(define (memv obj ls)
  (if (null? ls)
      #f
      (or (eq? obj (car ls))
          (memv obj (cdr ls)))))

(define (_equal? x y recursed)
  (cond ((memq x recursed) ; if already checked this exact object
         #t)               ; don't compare it again
        ((memq y recursed) ; same goes for y
         #t)
        ((eq? x y)
         #t)
        ((and (number? x) (number? y))
         (= x y))
        ((not (eq? (#$type x) (#$type y)))
         #f)
        ((null? x) #t)
        ((vector? x)
         (set! recursed (cons x (cons y recursed)))
         (all? (vector->list (vector-map (lambda (a b)
                                           (_equal? a b recursed))
                                         x y))))
        ((pair? x)
         (set! recursed (cons x (cons y recursed)))
         (and (_equal? (car x) (car y) recursed)
              (_equal? (cdr x) (cdr y) recursed)))
        ((string? x) (string=? x y))
        ((bytevector? x)
         (let/cc return
          (do ((i 0 (1+ i)))
              ((= i (bytevector-length x)) #t)
            (unless (_equal? (bytevector-u8-ref x i)
                             (bytevector-u8-ref y i)
                             recursed)
              (return #f)))))
        (else (eqv? x y))))

(define (equal? x y)
  (_equal? x y '()))

;; utility

;; apply the given function to pairs of the given list and return the results as
;; a list.
;;
;; for example, (pairwise list '(1 2 3 4)) would result in (1 2) (2 3) (3 4)
(define (pairwise fn ls)
  (cond ((< (length ls) 2)
         (error "Invalid number of arguments for pairwise"))
        ((eqv? (length ls) 2)
         (list (fn (car ls) (cadr ls))))
        (else
         (cons (fn (car ls) (cadr ls))
               (pairwise fn (cdr ls))))))

(define (reduce func values)
  (cond ((null? values)
         (func))
        ((null? (cdr values))
         (func (car values)))
        ((null? (cddr values))
         (func (car values) (cadr values)))
        (else (reduce func (cons (func (car values) (cadr values))
                                 (cddr values))))))

;; arithmetic

(define (+ . r)
  (if (null? r)
      0
      (#$iadd (car r) (apply + (cdr r)))))

(define (- . r)
  (if (null? r)
      (error "Invalid number of arguments for -")
      (if (null? (cdr r))
          (#$isub 0 (car r))
          (if (null? (cddr r))
              (#$isub (car r) (cadr r))
              (#$isub (apply - (butlast r)) (last r))))))

(define (* . r)
  (if (null? r)
      1
      (#$imul (car r) (apply * (cdr r)))))

(define (/ . r)
  (if (null? r)
      (error "Invalid number of arguments for /")
      (if (null? (cdr r))
          (#$idiv 1 (car r))
          (if (null? (cddr r))
              (#$idiv (car r) (cadr r))
              (#$idiv (apply / (butlast r)) (last r))))))

(define (remainder a b)
  (#$irem a b))

(define (modulo a b)
  (let ((res (remainder a b)))
    (if (< b 0)
        (if (<= res 0) res (+ res b))
        (if (>= res 0) res (+ res b)))))

(define (1+ n)
  (+ n 1))

(define (1- n)
  (- n 1))

;; quasiquote

(define qq-simplify-enabled #t)

;; we'll use these unique symbols when we want to generate list, append, quote,
;; and the like, so that when simplifying, we'll only process the values we
;; generated, and not the ones passed into the macro.
(define qq-quote (gensym "quote"))
(define qq-list (gensym "list"))
(define qq-list* (gensym "list*"))
(define qq-append (gensym "append"))
(define qq-list->vector (gensym "list->vector"))
(define qq-quote-nil (list qq-quote '()))

(define (qq-is-unquote form)
  (cond ((atom? form) #f)
        ((null? form) #f)
        ((eq? (car form) 'unquote) #t)
        (else #f)))

(define (qq-is-unquote-splicing form)
  (cond ((atom? form) #f)
        ((eq? (car form) 'unquote-splicing) #t)
        (else #f)))

(define (qq-is-quasiquote form)
  (cond ((atom? form) #f)
        ((null? form) #f)
        ((eq? (car form) 'quasiquote) #t)
        (else #f)))

(define (qq-process-list-item form level)
  (cond ((vector? form)
         (list qq-list (list qq-list->vector (qq-process-list (vector->list form) level))))
        ((atom? form)
         (list qq-quote (list form)))
        ((qq-is-unquote form)
         (if (eq? level 1)
             (list qq-list (cadr form))
             (list qq-list (qq-process-list form (- level 1)))))
        ((qq-is-unquote-splicing form)
         (if (eq? level 1)
             (cadr form)
             (list qq-list (qq-process-list form (- level 1)))))
        ((qq-is-quasiquote form)
         (list qq-list
               (qq-process-list form (+ level 1))))
        (else
         (list qq-list
               (qq-process-list form level)))))

(define (qq-process-list-tail form level)
  (cond ((vector? form)
         (list qq-list->vector (qq-process-list (vector->list form) level)))
        ((atom? form)
         (list qq-quote form))
        ((qq-is-unquote form)
         (if (eq? level 1)
             (cadr form)
             (qq-process-list form (- level 1))))
        ((qq-is-unquote-splicing form)
         (error "unquote-splicing in dotted tail"))
        ((qq-is-quasiquote form)
         (qq-process-list form (+ level 1)))
        (else
         (qq-process-list form level))))

(define (qq-split-improper-tail ls)
  ;; () => (() ())
  ;; (a) => ((a) ())
  ;; (a b) => ((a b) ())
  ;; (a . b) => ((a) b)
  ;; (a . ,b) => ((a) ,b)
  ;; (a b . ,c) => ((a b) ,c)
  ;; (a b . c) => ((a b) c)
  (cond ((null? ls) (list '() '()))
        ((atom? (cdr ls))
         (list (list (car ls)) (cdr ls)))
        ((qq-is-unquote (cdr ls))
         (list (list (car ls)) (cdr ls))) ;; same as atom? case
        ((qq-is-unquote-splicing (cdr ls))
         (list (list (car ls)) (cdr ls))) ;; same as atom? case
        (else
         (let ((split (qq-split-improper-tail (cdr ls))))
           (let ((rest (car split))
                 (tail (cadr split)))
             (list (cons (car ls) rest) tail))))))

(define (qq-process-list form level)
  (let ((rest/tail (qq-split-improper-tail form)))
    (let ((rest (car rest/tail))
          (tail (cadr rest/tail)))
      (let ((append-form (cons qq-append
                               (mapcar (lambda (form)
                                         (qq-process-list-item form level))
                                       rest))))
        (if (null? tail)
            append-form
            (append append-form (cons (qq-process-list-tail tail level) '())))))))

(define (qq-process form level)
  (cond ((vector? form)
         (list qq-list->vector (qq-process-list (vector->list form) level)))
        ((atom? form)
         (if (= level 1)
             (list qq-quote form)
             form))
        ((qq-is-unquote form)
         (if (= level 1)
             (cadr form)
             (qq-process-list form (- level 1))))
        ((qq-is-unquote-splicing form)
         (error "unquote-splicing immediately inside quasiquote."))
        ((qq-is-quasiquote form)
         (qq-process-list form (+ level 1)))
        (else
         (qq-process-list form level))))

(define (qq-maptree fn x)
  (if (atom? x)
      (fn x)
      (let ((a (fn (car x)))
            (d (qq-maptree fn (cdr x))))
        (if (eqv? a (car x))
            (if (eqv? d (cdr x))
                x
                (cons a d))
            (cons a d)))))

(define (qq-remove-tokens x)
  (cond ((eq? x qq-list) 'list)
        ((eq? x qq-append) 'append)
        ((eq? x qq-list*) 'list*)
        ((eq? x qq-quote) 'quote)
        ((eq? x qq-list->vector) 'list->vector)
        ((atom? x) x)
        ((and (eq? (car x) qq-list*)
              (pair? (cddr x))
              (null? (cdddr x)))
         (cons 'cons (qq-maptree qq-remove-tokens (cdr x))))
        (else (qq-maptree qq-remove-tokens x))))

(define (qq-simplify x)
  (if (atom? x)
      x
      (let ((x (if (eq? (car x) qq-quote)
                   x
                   (qq-maptree qq-simplify x))))
        (if (not (eq? (car x) qq-append))
            x
            (qq-simplify-args x)))))

(define (qq-simplify-args x)
  (let loop ((args (reverse (cdr x)))
             (result '()))
    (if (null? args)
        result
        (loop (cdr args)
              (cond ((atom? (car args))
                     (qq-attach-append qq-append (car args) result))
                    ((and (eq? (caar args) qq-list)
                          (not (any? (map qq-splicing-frob? (cdar args)))))
                     (qq-attach-conses (cdar args) result))
                    ((and (eq? (caar args) qq-list*)
                          (not (any? (map qq-splicing-frob? (cdar args)))))
                     (qq-attach-conses
                      (reverse (cdr (reverse (cdar args))))
                      (qq-attach-append qq-append
                                        (car (last (car args)))
                                        result)))
                    ((and (eq? (caar args) qq-quote)
                          (pair? (cadar args))
                          (not (qq-frob? (cadar args)))
                          (null? (cddar args)))
                     (qq-attach-conses (list (list qq-quote
                                                   (caadar args)))
                                       result))
                    (else (qq-attach-append qq-append
                                            (car args)
                                            result)))))))

(define (qq-attach-conses items result)
  (cond ((and (all? (map null-or-quoted? items))
              (null-or-quoted? result))
         (list qq-quote
               (append (mapcar cadr items) (cl-cadr result))))
        ((or (null? result) (equal? result qq-quote-nil))
         (cons qq-list items))
        ((and (pair? result)
              (or (eq? (car result) qq-list)
                  (eq? (car result) qq-list*)))
         (cons (car result) (append items (cdr result))))
        (else (cons qq-list* (append items (list result))))))

(define (null-or-quoted? x)
  (or (null? x) (and (pair? x) (eq? (car x) qq-quote))))

(define (cl-cadr x)
  (if (null? x)
      '()
      (if (null? (cdr x))
          '()
          (car (cdr x)))))

(define (qq-splicing-frob? x)
  (and (pair? x)
       (eq? (car x) 'unquote-splicing)))

(define (qq-frob? x)
  (and (pair? x)
       (or (eq? (car x) 'unquote)
           (eq? (car x) 'unquote-splicing))))

(define (qq-attach-append op item result)
  (cond ((and (null-or-quoted? item) (null-or-quoted? result))
         (list qq-quote (append (cl-cadr item) (cl-cadr result))))
        ((or (null? result) (equal? result qq-quote-nil))
         (if (qq-splicing-frob? item) (list op item) item))
        ((and (pair? result) (eq? (car result) op))
         (list* (car result) item (cdr result)))
        (else (list op item result))))

(define-macro (quasiquote form)
  (let ((raw-result (qq-process form 1)))
    (qq-remove-tokens (if qq-simplify-enabled
                          (qq-simplify raw-result)
                          raw-result))))

;;

(define (for-each proc . arg-lists)
  ;; we can't use map here because map's results depend on the order of argument
  ;; evaluation, which is unspecified in scheme, and in our implementation is
  ;; actually from right to left.
  (call/cc
   (lambda (exit)
     (when (null? arg-lists)
       (exit #f))
     (let loop ((arg-lists arg-lists))
       (when (any? (mapcar null? arg-lists))
         (exit #f))
       (apply proc (mapcar car arg-lists))
       (loop (mapcar cdr arg-lists))))))

(define assoc
  (case-lambda
   ((obj alist) (assoc obj alist equal?))
   ((obj alist compare)
    (if (null? alist)
        #f
        (if (compare obj (caar alist))
            (car alist)
            (assoc obj (cdr alist) compare))))))

(define (assq obj alist)
  (assoc obj alist eq?))

(define (assv obj alist)
  (assoc obj alist eqv?))

;; looks up obj in the given property list, using eq?
;; if obj is not found, #f is returned
;; if plist has an odd number of values (not a valid plist), the last
;; argument is silently ignored.
(define (plist-getq obj plist)
  (let loop ((plist plist))
    (if (and (pair? plist)
             (pair? (cdr plist)))
        (begin
          (if (eq? obj (car plist))
              (cadr plist)
              (loop (cddr plist))))
        #f)))

;; more list operations

(define (length ls)
  (cond ((null? ls) 0)
        ((not (pair? ls))
         (error "length: argument not a list"))
        ((null? (cdr ls))
         1)
        ((not (pair? (cdr ls)))
         (error "length: argument not a proper list"))
        (else (+ 1 (length (cdr ls))))))

(define (range1 start n acc)
  (if (<= n start)
      acc
      (range1 start (1- n) (cons (1- n) acc))))

(define (range start end)
  (range1 start end '()))

(define (iota n)
  (range 0 n))

(define (list-tail ls k)
  (if (zero? k)
      ls
      (list-tail (cdr ls) (1- k))))

(define (list-ref ls k)
  (car (list-tail ls k)))

(define (split-improper-tail ls)
  ;; () => (() ())
  ;; (a) => ((a) ())
  ;; (a b) => ((a b) ())
  ;; (a . b) => ((a) b)
  ;; (a b . c) => ((a b) c)
  (cond ((null? ls) '(() . ())) ;(list '() '()))
        ((atom? (cdr ls))
         (cons (list (car ls)) (cdr ls)))
        (else
         (let ((split (split-improper-tail (cdr ls))))
           (let ((rest (car split))
                 (tail (cdr split)))
             (cons (cons (car ls) rest) tail))))))

(define (proper-length ls)
  (cond ((null? ls) 0)
        ((not (pair? ls))
         (error "proper-length: argument not a list"))
        ((null? (cdr ls))
         1)
        ((not (pair? (cdr ls)))
         1)
        (else (1+ (proper-length (cdr ls))))))

;; characters

(define (char=? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise eqv? chars))))

(define (char<? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise < (map char->integer chars)))))

(define (char>? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise > (map char->integer chars)))))

(define (char<=? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise <= (map char->integer chars)))))

(define (char>=? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise >= (map char->integer chars)))))

(define (char-alphabetic? char)
  (let ((cat (char-general-category char)))
    (or (eq? cat 'Lu)
        (eq? cat 'Ll)
        (eq? cat 'Lo))))

(define (char-numeric? char)
  (eq? 'Nd (char-general-category char)))

(define (char-whitespace? char)
  (let ((cat (char-general-category char)))
    (or (eq? cat 'Zs)
        (eq? cat 'Zl)
        (eq? cat 'Zp)
        (eq? cat 'Cc))))

(define (char-upper-case? char)
  (eq? 'Lu (char-general-category char)))

(define (char-lower-case? char)
  (eq? 'Ll (char-general-category char)))

(define (char-ci=? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise char=? (map char-foldcase chars)))))

(define (char-ci<? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise char<? (map char-foldcase chars)))))

(define (char-ci>? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise char>? (map char-foldcase chars)))))

(define (char-ci<=? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise char<=? (map char-foldcase chars)))))

(define (char-ci>=? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise char>=? (map char-foldcase chars)))))

;; strings

(define (string1 s chars i)
  (if (null? chars)
      s
      (begin
        (string-set! s i (car chars))
        (string1 s (cdr chars) (1+ i)))))

(define (string . chars)
  (let ((s (make-string (length chars))))
    (string1 s chars 0)))

(define (string->list . args)
  (if (or (null? args) (> (length args) 3))
      (error "string->list accepts 1-3 arguments."))
  (let* ((str (car args))
         (strlen (string-length str))
         (start (if (>= (length args) 2)
                    (cadr args)
                    0))
         (end (if (>= (length args) 3)
                  (caddr args)
                  strlen)))
    (map (lambda (n) (string-ref str n))
         (range start end))))

(define (list->string ls)
  (apply string ls))

(define (string=?1 s1 s2)
  (and (eqv? (string-length s1) (string-length s2))
       (all? (map char=? (string->list s1) (string->list s2)))))

(define (string=? . strings)
  (if (null? (cdr strings))
      #t
      (all? (pairwise string=?1 strings))))

(define (substring str start end)
  (let ((result (make-string (- end start))))
    (do ((i start (1+ i))
         (j 0 (1+ j)))
        ((= i end) result)
      (string-set! result j (string-ref str i)))))

(define string-copy
  (case-lambda
   ((str) (substring str 0 (string-length str)))
   ((str start) (substring str start (string-length str)))
   ((str start end) (substring str start end))))

(define (_string-copy! to at from start end)
  (let ((n (- end start)))
    (do ((from-idx start (1+ from-idx))
         (to-idx at (1+ to-idx)))
        ((= from-idx end) to)
      (string-set! to to-idx (string-ref from from-idx)))))

(define string-copy!
  (case-lambda
   ((to at from) (_string-copy! to at from 0 (string-length from)))
   ((to at from start) (_string-copy! to at from start (string-length from)))
   ((to at from start end) (_string-copy! to at from start end))))

(define (_string-append s1 s2)
  (let ((result (make-string (+ (string-length s1) (string-length s2)))))
    (string-copy! result 0 s1)
    (string-copy! result (string-length s1) s2)))

(define string-append
  (case-lambda
   (() "")
   ((s) (string-copy s))
   ((s1 s2) (_string-append s1 s2))
   ((s1 s2 . rest) (_string-append (_string-append s1 s2)
                                   (apply string-append rest)))))

(define (_string-fill! str fill start end)
  (do ((i start (1+ i)))
      ((= i end) str)
    (string-set! str i fill)))

(define string-fill!
  (case-lambda
   ((str fill) (_string-fill! str fill 0 (string-length str)))
   ((str fill start) (_string-fill! str fill start (string-length str)))
   ((str fill start end) (_string-fill! str fill start end))))

;; vectors

(define (make-vector . args)
  (if (null? (cdr args))
      (#$make-vector (car args) #f)
      (#$make-vector (car args) (cadr args))))

(define (vector-map proc . args)
  (let ((result (make-vector (vector-length (car args)))))
    (do ((i 0 (1+ i)))
        ((= i (vector-length result)) result)
      (vector-set! result i (apply proc (mapcar (lambda (x) (vector-ref x i)) args))))))

(define (vector->list vec)
  (let ((result '()))
    (let loop ((i 0)
               (result '()))
      (if (< i (vector-length vec))
          (loop (1+ i) (cons (vector-ref vec i) result))
          (reverse result)))))

(define (list->vector ls)
  (do ((i 0 (1+ i))
       (ls ls (cdr ls))
       (vec (make-vector (length ls))))
      ((null? ls) vec)
    (vector-set! vec i (car ls))))

(define (vector-set! vec k obj)
  ;; this is needed because the SECD instruction issued for #$vector-set has a
  ;; different order of arguments that vector-set! should. We _could_ change the
  ;; instruction, but then the code generation for vector literals would become
  ;; rather awkward, involving some stack juggling with "swap".
  (#$vector-set! obj k vec))

(define (vector . objs)
  (list->vector objs))

(define (_vector->string vector start end)
  (let ((n (- end start)))
    (let loop ((s (make-string n))
               (vidx start)
               (sidx 0))
      (if (= sidx n)
          s
          (begin
            (string-set! s sidx (vector-ref vector vidx))
            (loop s (1+ vidx) (1+ sidx)))))))

(define vector->string
  (case-lambda
   ((vector) (_vector->string vector 0 (vector-length vector)))
   ((vector start) (_vector->string vector start (vector-length vector)))
   ((vector start end) (_vector->string vector start end))))

(define (_string->vector str start end)
  (let ((n (- end start)))
    (let loop ((s (make-vector n))
               (vidx 0)
               (sidx start))
      (if (= vidx n)
          s
          (begin
            (vector-set! s vidx (string-ref str sidx))
            (loop s (1+ vidx) (1+ sidx)))))))

(define string->vector
  (case-lambda
   ((str) (_string->vector str 0 (string-length str)))
   ((str start) (_string->vector str start (string-length str)))
   ((str start end) (_string->vector str start end))))

(define (_vector-fill! vector fill start end)
  (do ((i start (1+ i)))
      ((= i end) vector)
    (vector-set! vector i fill)))

(define vector-fill!
  (case-lambda
   ((vector fill) (_vector-fill! vector fill 0 (vector-length vector)))
   ((vector fill start) (_vector-fill! vector fill start (vector-length vector)))
   ((vector fill start end) (_vector-fill! vector fill start end))))

(define (_vector-copy vector start end)
  (let ((n (- end start)))
    (do ((r (make-vector n))
         (vidx start (1+ vidx))
         (ridx 0 (1+ ridx)))
        ((= ridx n) r)
      (vector-set! r ridx (vector-ref vector vidx)))))

(define vector-copy
  (case-lambda
   ((vector) (_vector-copy vector 0 (vector-length vector)))
   ((vector start) (_vector-copy vector start (vector-length vector)))
   ((vector start end) (_vector-copy vector start end))))

(define (_vector-copy! to at from start end)
  (let ((n (- end start)))
    (do ((from-idx start (1+ from-idx))
         (to-idx at (1+ to-idx)))
        ((= from-idx end) to)
      (vector-set! to to-idx (vector-ref from from-idx)))))

(define vector-copy!
  (case-lambda
   ((to at from) (_vector-copy! to at from 0 (vector-length from)))
   ((to at from start) (_vector-copy! to at from start (vector-length from)))
   ((to at from start end) (_vector-copy! to at from start end))))

(define (_vector-append v1 v2)
  (let ((result (make-vector (+ (vector-length v1) (vector-length v2)))))
    (vector-copy! result 0 v1)
    (vector-copy! result (vector-length v1) v2)))

(define vector-append
  (case-lambda
   (() #())
   ((v) (vector-copy v))
   ((v1 v2) (_vector-append v1 v2))
   ((v1 v2 . rest) (_vector-append (_vector-append v1 v2)
                                   (apply vector-append rest)))))

;; bytevectors

(define (make-bytevector . args)
  (if (null? (cdr args))
      (#$make-bytevector (car args) 0)
      (#$make-bytevector (car args) (cadr args))))

(define (bytevector-u8-set! bv k obj)
  ;; this is needed because the SECD instruction issued for #$bytevector-set!
  ;; has a different order of arguments that vector-set! should. We _could_
  ;; change the instruction, but then the code generation for vector literals
  ;; would become rather awkward, involving some stack juggling with "swap".
  (#$bytevector-u8-set! obj k bv))

(define (bytevector . objs)
  (let ((bv (make-bytevector (length objs))))
    (do ((i 0 (1+ i))
         (objs objs (cdr objs)))
        ((null? objs) bv)
      (bytevector-u8-set! bv i (car objs)))))

(define (_bv-copy bv start end)
  (let ((n (- end start)))
    (do ((r (make-bytevector n))
         (vidx start (1+ vidx))
         (ridx 0 (1+ ridx)))
        ((= ridx n) r)
      (bytevector-u8-set! r ridx (bytevector-u8-ref bv vidx)))))

(define bytevector-copy
  (case-lambda
   ((bv) (_bv-copy bv 0 (bytevector-length bv)))
   ((bv start) (_bv-copy bv start (bytevector-length bv)))
   ((bv start end) (_bv-copy bv start end))))

(define (_bv-copy! to at from start end)
  (let ((n (- end start)))
    (do ((from-idx start (1+ from-idx))
         (to-idx at (1+ to-idx)))
        ((= from-idx end) to)
      (bytevector-u8-set! to to-idx (bytevector-u8-ref from from-idx)))))

(define bytevector-copy!
  (case-lambda
   ((to at from) (_bv-copy! to at from 0 (bytevector-length from)))
   ((to at from start) (_bv-copy! to at from start (bytevector-length from)))
   ((to at from start end) (_bv-copy! to at from start end))))

(define (_bv-append bv1 bv2)
  (let ((result (make-bytevector (+ (bytevector-length bv1)
                                    (bytevector-length bv2)))))
    (bytevector-copy! result 0 bv1)
    (bytevector-copy! result (bytevector-length bv1) bv2)))

(define bytevector-append
  (case-lambda
   (() #u8())
   ((bv) (bytevector-copy bv))
   ((bv1 bv2) (_bv-append bv1 bv2))
   ((bv1 bv2 . rest) (_bv-append (_bv-append bv1 bv2)
                                 (apply bytevector-append rest)))))

(define utf8->string
  (case-lambda
   ((bv) (#$/str/fromutf8 bv 0 (bytevector-length bv)))
   ((bv start) (#$/str/fromutf8 bv start (bytevector-length bv)))
   ((bv start end) (#$/str/fromutf8 bv start end))))

(define string->utf8
  (case-lambda
   ((s) (#$/str/toutf8 s 0 (string-length s)))
   ((s start) (#$/str/toutf8 s start (string-length s)))
   ((s start end) (#$/str/toutf8 s start end))))

;; values

(define (values . things)
  (call/cc (lambda (k) (apply k things))))

(define (call-with-values producer consumer)
  (let ((vals (#$values->list (producer))))
    (apply consumer vals)))

;; ====== new call/cc and dynamic-wind
;; base on the implementation here: https://www.scheme.com/tspl4/control.html#./control:s56

(define winders '())

(define (common-tail x y)
  (let ((lx (length x))
        (ly (length y)))
    (do ((x (if (> lx ly) (list-tail x (- lx ly)) x)
            (cdr x))
         (y (if (> ly lx) (list-tail y (- ly lx)) y)
            (cdr y)))
        ((eq? x y) x))))

(define (do-wind new)
  (let ((tail (common-tail new winders)))
    ;; we'll loop on winders, starting from the first element and moving forward
    ;; until we reach the common tail.
    (let f ((ls winders))
      (unless (eq? ls tail)
        (set! winders (cdr ls))
        ((cdar ls)) ; call out-guard
        (f (cdr ls))))
    ;; now we'll loop over the new list of winders, starting from right before
    ;; the last non-common element, moving backwards to the first element,
    ;; calling in-guards.
    (let f ((ls new))
      (unless (eq? ls tail)
        (f (cdr ls))
        ((caar ls)) ; call in-guard
        (set! winders ls)))))

(define (call/cc f)
  (#$call/cc
   (lambda (k)
     (f (let ((save winders))
          (lambda x
            (unless (eq? save winders)
              (do-wind save))
            (apply k x)))))))

(define (call-with-current-continuation f)
  (call/cc f))

(define (dynamic-wind in body out)
  (in)
  (set! winders (cons (cons in out) winders))
  (let-values ((ans* (body)))
    (set! winders (cdr winders))
    (out)
    (apply values ans*)))

;; parameters

(define make-parameter
  (case-lambda
   ((init) (make-parameter init (lambda (x) x)))
   ((init converter) (let ((value (converter init)))
                       (case-lambda
                        (() value)
                        ((new-value) (set! value (converter new-value)))
                        ((new-value dont-convert) (if dont-convert
                                                      (set! value new-value)
                                                      (set! value (converter new-value)))))))))

;; io

(define current-input-port (make-parameter (#$/io/stdin)))
(define current-output-port (make-parameter (#$/io/stdout)))
(define current-error-port (make-parameter (#$/io/stderr)))

(define write-char
  (case-lambda
   ((char) (write-char char (current-output-port)))
   ((char port) (#$/io/write (#$/str/format 'simple 'display char) port))))

(define write-u8
  (case-lambda
   ((byte) (write-u8 byte (current-output-port)))
   ((byte port) (#$/io/writebv (bytevector byte) port))))

(define write-string
  (case-lambda
   ((str) (write-string str (current-output-port) 0 (string-length str)))
   ((str port) (write-string str port 0 (string-length str)))
   ((str port start) (write-string str port start (string-length str)))
   ((str port start end) (#$/io/write (substring str start end) port))))

(define write-bytevector
  (case-lambda
   ((bv) (write-bytevector bv (current-output-port) 0 (bytevector-length bv)))
   ((bv port) (write-bytevector bv port 0 (bytevector-length bv)))
   ((bv port start) (write-bytevector bv port start (bytevector-length bv)))
   ((bv port start end) (#$/io/writebv (bytevector-copy bv start end) port))))

(define write
  (case-lambda
   ((obj) (write obj (current-output-port)))
   ((obj port)
    (let ((str (#$/str/format 'cyclic 'write obj)))
      (#$/io/write str port)))))

(define write-shared
  (case-lambda
   ((obj) (write obj (current-output-port)))
   ((obj port)
    (let ((str (#$/str/format 'shared 'write obj)))
      (#$/io/write str port)))))

(define write-simple
  (case-lambda
   ((obj) (write obj (current-output-port)))
   ((obj port)
    (let ((str (#$/str/format 'simple 'write obj)))
      (#$/io/write str port)))))

(define newline
  (case-lambda
   (() (newline (current-output-port)))
   ((port) (write-char #\newline))))

(define display
  (case-lambda
   ((obj) (display obj (current-output-port)))
   ((obj port)
    (let ((str (#$/str/format 'cyclic 'display obj)))
      (#$/io/write str port)))))

(define flush-output-port
  (case-lambda
   (() (flush-output-port (current-output-port)))
   ((port) (#$/io/flush port))))

(define (print . objs)
  (do ((objs objs (cdr objs))
       (dummy 0 (write-char #\space)))
      ((null? objs) (newline))
    (write (car objs))))

(define (open-input-file filename)
  (#$/io/open filename "r"))

(define (open-binary-input-file filename)
  (#$/io/open filename "rb"))

(define (open-output-file filename)
  (#$/io/open filename "w"))

(define (open-binary-output-file filename)
  (#$/io/open filename "wb"))

(define (open-input-string s)
  (#$/io/openistr s))

(define (open-input-bytevector bv)
  (#$/io/openibv bv))

(define (open-output-string)
  (#$/io/openostr))

(define (open-output-bytevector)
  (#$/io/openobv))

(define (get-output-string port)
  (#$/io/portstr port))

(define (get-output-bytevector port)
  (#$/io/portbv port))

(define read-string
  (case-lambda
   ((n) (read-string n (current-input-port)))
   ((n port) (let ((r (#$/io/read port n)))
               (if (and (positive? n) (not (string=? "" r)))
                   r
                   (eof-object))))))

(define read-u8
  (case-lambda
   (() (read-u8 (current-input-port)))
   ((port) (let ((r (#$/io/readbyte port)))
             (if (negative? r)
                 (eof-object)
                 r)))))

(define read-bytevector
  (case-lambda
   ((n) (read-bytevector n (current-input-port)))
   ((n port) (let ((r (#$/io/readbv port n)))
               (if (and (positive? n) (zero? (bytevector-length r)))
                   (eof-object)
                   r)))))

(define read-bytevector!
  (case-lambda
   ((bv) (read-bytevector! bv (current-input-port) 0 (bytevector-length bv)))
   ((bv port) (read-bytevector! bv port 0 (bytevector-length bv)))
   ((bv port start) (read-bytevector! bv port start (bytevector-length bv)))
   ((bv port start end) (let* ((n (- end start))
                               (r (#$/io/readbv port n)))
                          (if (and (positive? n) (zero? (bytevector-length r)))
                              (eof-object)
                              (begin
                                (bytevector-copy! bv start r 0 (bytevector-length r))
                                (bytevector-length r)))))))

(define read-char
  (case-lambda
   (() (read-char (current-input-port)))
   ((port) (let ((r (#$/io/read port 1)))
             (if (string=? r "")
                 (eof-object)
                 (string-ref r 0))))))

(define read-line
  (case-lambda
   (() (read-line (current-input-port)))
   ((port) (let ((r (#$/io/readline port)))
             (if (string=? r "")
                 (eof-object)
                 (if (eqv? #\newline (string-ref r (1- (string-length r))))
                     (string-copy r 0 (1- (string-length r)))
                     r))))))

(define (input-port? port)
  (eq? 'input (#$/io/portdir port)))

(define (output-port? port)
  (eq? 'output (#$/io/portdir port)))

(define (textual-port? port)
  (eq? 'text (#$/io/portmode port)))

(define (binary-port? port)
  (eq? 'binary (#$/io/portmode port)))

(define (close-input-port port)
  (unless (input-port? port)
    (raise (file-error "Not an input port")))
  (close-port port))

(define (close-output-port port)
  (unless (output-port? port)
    (raise (file-error "Not an output port")))
  (close-port port))

(define (close-port port)
  (#$/io/close port))

(define (input-port-open? port)
  (if (input-port? port)
      (#$/io/isopen port)
      #f))

(define (output-port-open? port)
  (if (output-port? port)
      (#$/io/isopen port)
      #f))

(define (flush-output-port port)
  (#$/io/flush port))

(define (call-with-port port proc)
  (let ((result (proc port)))
    (close-port port)
    result))

(define (call-with-input-file filename proc)
  (let ((port (open-input-file filename)))
    (call-with-port port proc)))

(define (call-with-output-file filename proc)
  (let ((port (open-output-file filename)))
    (call-with-port port proc)))

(define (with-input-from-file filename thunk)
  (let ((port (open-input-file filename)))
    (parameterize ((current-input-port port))
      (thunk)
      (close-input-port port))))

(define (with-output-to-file filename thunk)
  (let ((port (open-output-file filename)))
    (parameterize ((current-output-port port))
      (thunk)
      (close-output-port port))))

(define (file-exists? filename)
  (#$/io/exists filename))

(define (delete-file filename)
  (#$/io/delete filename))

;; read

(define read
  (case-lambda
   (() (read (current-input-port)))
   ((port) (let ((r (#$/read/read port)))
             (if (void? r)
                 (eof-object)
                 r)))))

;; record types

(define-macro (define-record-type name constructor pred . fields)
  (define get-field-accessors
    (case-lambda
     ((i field-name getter) `((define (,getter rec)
                                (vector-ref (#$unwrap rec) ,i))))
     ((i field-name getter setter) `((define (,getter rec)
                                       (vector-ref (#$unwrap rec) ,i))
                                     (define (,setter rec val)
                                       (vector-set! (#$unwrap rec) ,i val))))))
  (let ((type-id (gensym (symbol->string name))))
    `(begin
       (define (,pred x) (eq? (#$type x) ',type-id))
       (define ,constructor (#$wrap (vector ,@(cdr constructor)) ',type-id))
       ,@(let loop ((fields fields)
                    (i 0)
                    (results '()))
           (if (null? fields)
               results
               (loop (cdr fields)
                     (1+ i)
                     (append results
                             (apply get-field-accessors i (car fields)))))))))

;; exit

;; will be set further down
(define exit-continuation #f)

(define exit
  (case-lambda
   (() (exit 0))
   ((exit-code) (exit-continuation exit-code))))

(define emergency-exit
  (case-lambda
   (() (emergency-exit 0))
   ((exit-code) (#$/sys/exit (if exit-code exit-code 1))))) ;; if exit-code == #f, return 1 as exit code

;; capture a continuation that will exit the system when called again, and store
;; it in exit-continuation. we exit this way to make sure all dynamic-wind
;; out-guards are called before we exit.
(let* ((dont-exit (gensym))
       (code (call/cc (lambda (k)
                        (set! exit-continuation k)
                        dont-exit))))
  (unless (eq? code dont-exit)
    (emergency-exit code)))

;; exceptions

;; whenever a system exception happens, this function is called in the dynamic
;; environment in which the exception happened. we convert the passed arguments
;; into an error object and raise that.
(#$set-system-exception-handler
 (lambda (msg kind continuation)
   (define irritants (list 'context 'system
                           'continuation continuation))
   (raise (make-error msg irritants kind))))

(define exception-handlers '())

(define (terminate-with-exception e)
  (define (system-error? e)
    (eq? 'system (plist-getq 'context (error-object-irritants e))))

  ;; do we need to call the "after" procedures of any active dynamic-wind
  ;; invocations here? I'd argue that we don't. reading through the definition
  ;; of entering/exiting dynamic environment in r7rs small spec (page 53), i see
  ;; it's only described in terms of either calling/returning, or invoking
  ;; call/cc, neither of which is happening here.
  ;;
  ;; still, we _could_ call the "after" procedures here, and i don't think that
  ;; would be entirely against the spec, but there a few issues. what if there
  ;; is another unhandled exception when unwinding? we might go into an infinite
  ;; loop. one half-solution to that would be to clear the list of winders
  ;; before unwinding, but that's a risky maneuver that might break things.
  ;;
  ;; another risk of unwinding is that one of the unwinders might invoke another
  ;; continuation and never return, but that's undefined behavior according to
  ;; the spec ("the effect of using a captured continuation to enter or exit the
  ;; dynamic extent of a call to before or after is unspecified."), so i
  ;; wouldn't be too much worried about i could find a solution to the previous
  ;; issue.

  (if (error-object? e)
      (if (system-error? e)
          (#$abort (error-object-message e)
                   (plist-getq 'continuation (error-object-irritants e)))
          (#$abort (error-object-message e) #f))
      (#$abort e #f)))

(define (raise e)
  (when (null? exception-handlers)
    (terminate-with-exception e))

  (let ((old-handlers exception-handlers)
        (cur-handler #f))
    (dynamic-wind
        (lambda ()
          (set! old-handlers exception-handlers)
          (set! cur-handler (car exception-handlers))
          (set! exception-handlers (cdr exception-handlers)))
        (lambda ()
          (cur-handler e)
          (raise (error "an exception handler returned" 'wrapped-exception e)))
        (lambda ()
          (set! exception-handlers old-handlers)))))

(define (raise-continuable e)
  (when (null? exception-handlers)
    (raise
     (error "a continuable exception happened, but there was no error handler"
            'wrapped-exception e)))

  (let ((old-handlers exception-handlers)
        (cur-handler #f))
    (dynamic-wind
        (lambda ()
          (set! old-handlers exception-handlers)
          (set! cur-handler (car exception-handlers))
          (set! exception-handlers (cdr old-handlers)))
        (lambda ()
          (cur-handler e))
        (lambda ()
          (set! exception-handlers old-handlers)))))

(define (with-exception-handler handler thunk)
  (dynamic-wind
      (lambda ()
        (set! exception-handlers
              (cons handler exception-handlers)))
      (lambda () (thunk))
      (lambda ()
        (set! exception-handlers (cdr exception-handlers)))))

(define-record-type error
  (make-error message irritants kind)
  error-object?
  (message error-object-message)
  (irritants error-object-irritants)
  (kind error-object-kind))

(define (error msg . irritants)
  (raise (make-error msg irritants 'normal)))

(define (file-error? obj)
  (and (error-object? obj)
       (eq? 'file (error-object-kind obj))))

(define (file-error msg . irritants)
  (raise (make-error msg irritants 'file)))

(define (read-error? obj)
  (and (error-object? obj)
       (eq? 'read (error-object-kind obj))))

;; eof object

(define-record-type eof
  (eof-object)
  eof-object?)
