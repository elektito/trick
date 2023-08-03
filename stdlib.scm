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

(define (null? x) (eq? 'nil (type x)))

(define (atom? v)
  ;; everything besides cons (3) is an atom
  (if (eq? (type v) 'pair) #f #t))

(define (symbol? v) (eq? (type v) 'symbol))
(define (pair? v) (eq? (type v) 'pair))
(define (integer? v) (eq? (type v) 'int))
(define (string? v) (eq? (type v) 'string))
(define (char? v) (eq? (type v) 'char))
(define (procedure? v) (eq? (type v) 'procedure))
(define (boolean? v) (eq? (type v) 'bool))
(define (list? v)
  (if (null? v)
      #t
      (if (pair? v)
          (list? (cdr v))
          #f)))
(define (vector? v)
  (eq? (type v) 'vector))

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

;; cond

(define-macro (cond . arms)
  (if (null? arms)
      '(quote ())
      (list 'if
            (caar arms)
            (cons 'begin (cdar arms))
            (cons 'cond (cdr arms)))))

;;

(define-macro (and . forms)
  (cond ((null? forms) '#t)
        ((null? (cdr forms)) (car forms))
        (#t (list 'if
                  (car forms)
                  (cons 'and (cdr forms))
                  #f))))

(define-macro (or . forms)
  (cond ((null? forms) '#f)
        ((null? (cdr forms)) (car forms))
        (#t (let ((xcar (gensym)))
              (list 'let (list (list xcar (car forms)))
                    (list 'if xcar xcar (cons 'or (cdr forms))))))))

;; general comparison

(define (memq obj ls)
  (if (null? ls)
      #f
      (or (eq? obj (car ls))
          (memq obj (cdr ls)))))

(define (_equal? x y recursed)
  (cond ((memq x recursed) ; if already checked this exact object
         #t)               ; don't compare it again
        ((memq y recursed) ; same goes for y
         #t)
        ((eq? x y)
         #t)
        ((not (eq? (type x) (type y)))
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
        (#t (eqv? x y))))

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
        (#t
         (cons (fn (car ls) (cadr ls))
               (pairwise fn (cdr ls))))))

(define (reduce func values)
  (cond ((null? values)
         (func))
        ((null? (cdr values))
         (func (car values)))
        ((null? (cddr values))
         (func (car values) (cadr values)))
        (#t (reduce func (cons (func (car values) (cadr values))
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
        (#t #f)))

(define (qq-is-unquote-splicing form)
  (cond ((atom? form) #f)
        ((eq? (car form) 'unquote-splicing) #t)
        (#t #f)))

(define (qq-is-quasiquote form)
  (cond ((atom? form) #f)
        ((null? form) #f)
        ((eq? (car form) 'quasiquote) #t)
        (#t #f)))

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
        (#t
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
        (#t
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
        (#t
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
        (#t
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
        (#t (qq-maptree qq-remove-tokens x))))

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
                    (#t (qq-attach-append qq-append
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
        (#t (cons qq-list* (append items (list result))))))

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
        (#t (list op item result))))

(define-macro (quasiquote form)
  (let ((raw-result (qq-process form 1)))
    (qq-remove-tokens (if qq-simplify-enabled
                          (qq-simplify raw-result)
                          raw-result))))

;; more macros now that we have quasiquote!

(define-macro (when c . body)
  `(if ,c (begin ,@body)))

(define-macro (unless c . body)
  `(if ,c (#$void) (begin ,@body)))

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

;; case-lambda

(define (create-case-lambda-matcher formals)
  (let* ((rest/tail (split-improper-tail formals))
         (rest (car rest/tail))
         (tail (cdr rest/tail)))
    (cond ((symbol? formals)
           #t)
          ((null? tail)
           `(= ,(length formals) (length args)))
          (#t
           `(and (>= (proper-length args)
                     ,(length rest)))))))

(define (create-case-lambda-clause c)
  (let ((formals (car c))
        (body (cdr c)))
    (if (symbol? formals)
        `(#t (lambda ,formals ,@body))
        `(,(create-case-lambda-matcher formals)
          (lambda ,formals
            ,@body)))))

(define-macro (case-lambda . clauses)
  `(lambda args
     (apply
      (cond ,@(map create-case-lambda-clause clauses)
            (#t (lambda x (error "Argument list does not match any case-lambda clause"))))
      args)))

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

(define (assq obj alist)
  (if (null? alist)
      #f
      (if (eq? obj (caar alist))
          (car alist)
          (assq obj (cdr alist)))))

(define (assv obj alist)
  (assq obj alist))

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
        (#t (+ 1 (length (cdr ls))))))

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
        (#t
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
        (#t (1+ (proper-length (cdr ls))))))

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

;; receives the bindings list passed to let-values and returns an alist mapping
;; each variable name in it to a gensym.
(define (lv-map-vars bindings)
  (apply append
         (map (lambda (b)
                (let ((formals (car b)))
                  (if (symbol? formals)
                      (list (cons formals (gensym)))
                      (map (lambda (s) (cons s (gensym)))
                           formals))))
              bindings)))

;; given a list of bindings passed to let-values, and an alist created by
;; lv-map-vars, and returns a new bindings list in which all names are mapped to
;; their counterpart in the alist.
(define (lv-convert-to-temps bindings alist)
  (map (lambda (b)
         (let ((formals (car b)))
           (if (symbol? formals)
               (cons (cdr (assq formals alist)) (cdr b))
               (cons (map (lambda (f)
                            (cdr (assq f alist)))
                          formals)
                     (cdr b)))))
       bindings))

;; given an alist created by lv-map-vars, creates a let binding list that maps
;; names passed by the user to gensyms
(define (lv-create-rebindings alist)
  (map (lambda (pair)
         (list (car pair) (cdr pair)))
       alist))

(define-macro (let-values bindings . body)
  (let ((mapping (lv-map-vars bindings)))
    `(let*-values ,(lv-convert-to-temps bindings mapping)
       (let ,(lv-create-rebindings mapping)
         ,@body))))

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

(define-macro (parameterize bindings . body)
  (let* ((gensyms (map (lambda (x) (gensym)) bindings))
         (old-bindings (map (lambda (g b)
                              `(,g (,(car b))))
                            gensyms bindings))
         (set-new-bindings (map (lambda (g b)
                                  `((,(car b) ,(cadr b))))
                                gensyms bindings))
         (set-new-bindings (apply append set-new-bindings))
         (set-old-bindings (map (lambda (g b)
                                  `(,(car b) ,g #t))
                                gensyms bindings)))
    `(let (,@old-bindings)
       (dynamic-wind
           (lambda () ,@set-new-bindings)
           (lambda () ,@body)
           (lambda () ,@set-old-bindings)))))

;; io

(define current-input-port (make-parameter (#$/io/stdin)))
(define current-output-port (make-parameter (#$/io/stdout)))
(define current-error-port (make-parameter (#$/io/stderr)))

(define write-char
  (case-lambda
   ((char) (write-char char (current-output-port)))
   ((char port) (#$/io/write (#$/str/format 'simple 'display char) port))))

(define write-string
  (case-lambda
   ((str) (write-string str (current-output-port) 0 (string-length str)))
   ((str port) (write-string str port 0 (string-length str)))
   ((str port start) (write-string str port start (string-length str)))
   ((str port start end) (#$/io/write (substring str start end) port))))

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

;; record types

(define-macro (define-record-type name constructor pred . fields)
  (define get-field-accessors
    (case-lambda
     ((i field-name getter) `((define (,getter rec)
                                (vector-ref (unwrap rec) ,i))))
     ((i field-name getter setter) `((define (,getter rec)
                                       (vector-ref (unwrap rec) ,i))
                                     (define (,setter rec val)
                                       (vector-set! (unwrap rec) ,i val))))))
  (let ((type-id (gensym (symbol->string name))))
    `(begin
       (define (,pred x) (eq? (type x) ',type-id))
       (define ,constructor (wrap (vector ,@(cdr constructor)) ',type-id))
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
(set-system-exception-handler (lambda (msg irritants)
                                (apply error msg irritants)))

(define exception-handlers '())

(define (display-exception e)
  (if (error-object? e)
      (display (error-object-message e))
      (display e)))

(define (terminate-with-exception e)
  (display "Unhandled exception: ")
  (display-exception e)
  (newline)

  (let loop ((e e))
    (when (error-object? e)
      (let ((wrapped (plist-getq 'wrapped-exception
                                 (error-object-irritants e))))
        (when wrapped
          (display "This happened while handling another exception: ")
          (display-exception wrapped)
          (newline)
          (loop wrapped)))))

  (exit #f))

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
  (make-error message irritants)
  error-object?
  (message error-object-message)
  (irritants error-object-irritants))

(define (error msg . irritants)
  (raise (make-error msg irritants)))
