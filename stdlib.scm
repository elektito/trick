;; primcalls

(define (make-string . args)
  (if (null? (cdr args))
      (#$make-string (car args) #\null)
      (#$make-string (car args) (cadr args))))

(define (apply fn . args)
  (#$apply fn (concat (butlast args) (last args))))

;; type predicates

(define (null? x) (eq? 'nil (type x)))

(define (atom? v)
  ;; everything besides cons (3) is an atom
  (if (eq? (type v) 'pair) #f #t))

(define (symbol? v) (eq? (type v) 'symbol))
(define (pair? v) (eq? (type v) 'pair))
(define (integer? v) (eq? (type v) 'int))
(define (string? v) (eq? (type v) 'string))
(define (char? v) (eq? (type v) 'char))
(define (closure? v) (eq? (type v) 'closure))
(define (bool? v) (eq? (type v) 'bool))
(define (list? v)
  (if (null? v)
      #t
      (if (pair? v)
          (list? (cdr v))
          #f)))

;; booleans

(define (not x)
  (if x #f #t))

(define (boolean=? x y)
  (if x y (not y)))

;; symbols

(define (symbol=? x y)
  (eq? x y))

;; numeric comparison


(define (< x y)
  (#$ilt x y))
(define (<= x y)
  (#$ile x y))
(define (> x y)
  (not (<= x y)))
(define (>= x y)
  (not (< x y)))
(define (zero? x) (eqv? x 0))
(define (negative? x) (< x 0))
(define (positive? x) (> x 0))

(define (= . numbers)
  (if (null? (cdr numbers))
      #t
      (all? (pairwise eqv? numbers))))

;; list utilities

(define (list . values) values)

(define (concat2 l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (concat2 (cdr l1) l2))))

(define (concat1 lists)
  (if (null? lists)
      '()
      (concat2 (car lists)
               (concat1 (cdr lists)))))

(define (concat . lists)
  (concat1 lists))

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

;; begin, when, unless, cond

(define-macro (begin . body)
  (list (concat (list 'lambda '()) body)))

(define-macro (when c . body)
  `(if ,c (begin ,@body) #f))

(define-macro (unless c . body)
  `(if ,c #f (begin ,@body)))

(define-macro (cond . arms)
  (if (null? arms)
      '(quote ())
      (list 'if
            (caar arms)
            (cons 'begin (cdar arms))
            (cons 'cond (cdr arms)))))

;; utility

;; apply the given function to pairs of the given list and return the results as
;; a list.
;;
;; for example, (pairwise list '(1 2 3 4)) would result in (1 2) (2 3) (3 4)
(define (pairwise fn ls)
  (cond ((< (length ls) 2)
         (error :arg-error :msg "Invalid number of arguments for pairwise"))
        ((eqv? (length ls) 2)
         (list (fn (car ls) (cadr ls))))
        (#t
         (cons (fn (car ls) (cadr ls))
               (pairwise fn (cdr ls))))))

;; arithmetic

(define (+ . r)
  (if (null? r)
      0
      (#$iadd (car r) (apply + (cdr r)))))

(define (- . r)
  (if (null? r)
      (error :arg-error :msg "Invalid number of arguments for -")
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
      (error :arg-error :msg "Invalid number of arguments for /")
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

;; backquote

(define (bq-simplify form)
  ;; if there's an concat in which all arguments are lists of size 1, convert it
  ;; to a "list" call:
  ;; (concat '(x) (list y) '(z)) => (list 'x y 'z)
  ;;
  ;; if there is a list call in which all forms are quoted, convert it
  ;; to a single quoted list:
  ;; (list 'x 'y 'z) => '(x y z)
  form)

(define (bq-is-unquote form)
  (cond ((atom? form) #f)
        ((null? form) #f)
        ((eq? (car form) 'unquote) #t)
        (#t #f)))

(define (bq-is-unquote-splicing form)
  (cond ((atom? form) #f)
        ((eq? (car form) 'unquote-splicing) #t)
        (#t #f)))

(define (bq-is-backquote form)
  (cond ((atom? form) #f)
        ((null? form) #f)
        ((eq? (car form) 'backquote) #t)
        (#t #f)))

(define (bq-process-list-item form level)
  (cond ((atom? form)
         (list 'quote (list form)))
        ((bq-is-unquote form)
         (if (eq? level 1)
             (list 'list (cadr form))
             (list 'list (bq-process-list form (- level 1)))))
        ((bq-is-unquote-splicing form)
         (cadr form))
        ((bq-is-backquote form)
         (list 'list
               (bq-process-list form (+ level 1))))
        (#t
         (list 'list
               (bq-process-list form level)))))

(define (bq-process-list form level)
  (cons 'concat
        (mapcar (lambda (form)
                  (bq-process-list-item form level))
                form)))

(define (bq-process form level)
  (cond ((atom? form)
         (if (= level 1)
             (list 'quote form)
             form))
        ((bq-is-unquote form)
         (if (= level 1)
             (cadr form)
             (bq-process-list form (- level 1))))
        ((bq-is-unquote-splicing form)
         (error :backquote-error
                :msg "unquote-splicing immediately inside backquote."))
        ((bq-is-backquote form)
         (bq-process-list form (+ level 1)))
        (#t
         (bq-process-list form level))))

(define-macro (backquote form)
  (bq-simplify
   (bq-process form 1)))

;; more macros now that we have backquote!

(define-macro (with-gensyms names . body)
  `(let ,(mapcar (lambda (name) `(,name (gensym))) names)
     ,@body))

(define-macro (and . forms)
  (cond ((null? forms) '#t)
        ((null? (cdr forms)) (car forms))
        (#t `(if ,(car forms)
                 (and ,@(cdr forms))
                 '#f))))

(define-macro (or . forms)
  (cond ((null? forms) '#f)
        ((null? (cdr forms)) (car forms))
        (#t (with-gensyms (xcar)
              `(let ((,xcar ,(car forms)))
                 (if ,xcar ,xcar (or ,@(cdr forms))))))))

(define-macro (let* bindings . body)
  (if (null? bindings)
      `(begin ,@body)
      `(let (,(car bindings))
         (let* ,(cdr bindings) ,@body))))

(define-macro (do iteration-spec test-and-tail-seq . body)
  (let* ((test (car test-and-tail-seq))
         (tail-seq (cdr test-and-tail-seq))
         (var-names (map car iteration-spec))
         (var-inits (map cadr iteration-spec))
         (nexts (map (lambda (x)
                       (if (>= (length x) 3)
                           (caddr x)
                           (car x)))
                     iteration-spec))
         (bindings (map list var-names var-inits)))
    (with-gensyms (exit-gc let-gc)
      `(call/cc
        (lambda (,exit-gc)
          (let ,let-gc ,bindings
               (when ,test
                 (,exit-gc (begin ,@tail-seq)))
               ,@body
               (,let-gc ,@nexts)))))))

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

(define (append1 ls1 ls2)
  (if (null? ls1)
      ls2
      (if (null? ls2)
          ls1
          (cons (car ls1) (append1 (cdr ls1) ls2)))))

(define (append . lists)
  (if (null? lists)
      '()
      (append1 (car lists) (apply append (cdr lists)))))

(define (assq obj alist)
  (if (null? alist)
      #f
      (if (eq? obj (caar alist))
          (car alist)
          (assq obj (cdr alist)))))

(define (assv obj alist)
  (assq obj alist))

;; more list operations

(define (length ls)
  (cond ((null? ls) 0)
        ((not (pair? ls))
         (error :arg-error :msg "length: argument not a list"))
        ((null? (cdr ls))
         1)
        ((not (pair? (cdr ls)))
         (error :arg-error :msg "length: argument not a proper list"))
        (#t (+ 1 (length (cdr ls))))))

(define (range' start n acc)
  (if (<= n start)
      acc
      (range' start (1- n) (cons (1- n) acc))))

(define (range start end)
  (range' start end '()))

(define (iota n)
  (range 0 n))

(define (reverse1 ls acc)
  (if (null? ls)
      acc
      (reverse1 (cdr ls) (cons (car ls) acc))))

(define (reverse ls)
  (reverse1 ls '()))

;; general comparison

(define (eqv? x y)
  ;; for our current implementation, eq? already does the same thing as eqv?
  ;; should
  (eq? x y))

(define (equal? x y)
  (cond ((not (eq? (type x) (type y)))
         #f)
        ((null? x) #t)
        ((pair? x) (and (equal? (car x) (car y))
                        (equal? (cdr x) (cdr y))))
        ((string? x) (string=? x y))
        (#t (eqv? x y))))

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

(define (string' s chars i)
  (if (null? chars)
      s
      (begin
        (string-set! s i (car chars))
        (string' s (cdr chars) (1+ i)))))

(define (string . chars)
  (let ((s (make-string (length chars))))
    (string' s chars 0)))

(define (string->list . args)
  (if (or (null? args) (> (length args) 3))
      (error :arg-error :msg "string->list accepts 1-3 arguments."))
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

(define (string=?' s1 s2)
  (and (eqv? (string-length s1) (string-length s2))
       (all? (map char=? (string->list s1) (string->list s2)))))

(define (string=? . strings)
  (if (null? (cdr strings))
      #t
      (all? (pairwise string=?' strings))))

;; values

(define (values . things)
  (call/cc (lambda (k) (apply k things))))

(define (call-with-values producer consumer)
  (let ((vals (#$values->list (producer))))
    (apply consumer vals)))

(define-macro (let*-values bindings . body)
  (if (null? bindings)
      `(begin ,@body)
      `(call-with-values (lambda () ,(cadar bindings))
        (lambda ,(caar bindings)
          (let*-values ,(cdr bindings) ,@body)))))
