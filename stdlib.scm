;; primcalls

(define (make-string . args)
  (if (null? (cdr args))
      (#$make-string (car args) #\null)
      (#$make-string (car args) (cadr args))))

(define (apply fn . args)
  ;; last argument must be a list
  (#$apply fn (append (butlast args) (last args))))

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

;; begin and cond

(define-macro (begin . body)
  (list (append (list 'lambda '()) body)))

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

;; quasiquote

(define (qq-simplify form)
  ;; if there's an append in which all arguments are lists of size 1, convert it
  ;; to a "list" call:
  ;; (append '(x) (list y) '(z)) => (list 'x y 'z)
  ;;
  ;; if there is a list call in which all forms are quoted, convert it
  ;; to a single quoted list:
  ;; (list 'x 'y 'z) => '(x y z)
  form)

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
  (cond ((atom? form)
         (list 'quote (list form)))
        ((qq-is-unquote form)
         (if (eq? level 1)
             (list 'list (cadr form))
             (list 'list (qq-process-list form (- level 1)))))
        ((qq-is-unquote-splicing form)
         (cadr form))
        ((qq-is-quasiquote form)
         (list 'list
               (qq-process-list form (+ level 1))))
        (#t
         (list 'list
               (qq-process-list form level)))))

(define (qq-process-list-tail form level)
  (cond ((atom? form)
         (list 'quote form))
        ((qq-is-unquote form)
         (if (eq? level 1)
             (cadr form)
             (qq-process-list form (- level 1))))
        ((qq-is-unquote-splicing form)
         (error :quasiquote-error
                :msg "unquote-splicing in dotted tail"))
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
      (let ((append-form (cons 'append
                               (mapcar (lambda (form)
                                         (qq-process-list-item form level))
                                       rest))))
        (if (null? tail)
            append-form
            (append append-form (cons (qq-process-list-tail tail level) '())))))))

(define (qq-process form level)
  (cond ((vector? form)
         (list 'list->vector (qq-process (vector->list form) level)))
        ((atom? form)
         (if (= level 1)
             (list 'quote form)
             form))
        ((qq-is-unquote form)
         (if (= level 1)
             (cadr form)
             (qq-process-list form (- level 1))))
        ((qq-is-unquote-splicing form)
         (error :quasiquote-error
                :msg "unquote-splicing immediately inside quasiquote."))
        ((qq-is-quasiquote form)
         (qq-process-list form (+ level 1)))
        (#t
         (qq-process-list form level))))

(define-macro (quasiquote form)
  (qq-simplify
   (qq-process form 1)))

;; more macros now that we have quasiquote!

(define-macro (when c . body)
  `(if ,c (begin ,@body) #f))

(define-macro (unless c . body)
  `(if ,c #f (begin ,@body)))

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
           `(and (is-improper args)
                 (>= (proper-length args)
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
            (#t (lambda x (error :bad-args))))
      args)))

;;

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

(define (range1 start n acc)
  (if (<= n start)
      acc
      (range1 start (1- n) (cons (1- n) acc))))

(define (range start end)
  (range1 start end '()))

(define (iota n)
  (range 0 n))

(define (reverse1 ls acc)
  (if (null? ls)
      acc
      (reverse1 (cdr ls) (cons (car ls) acc))))

(define (reverse ls)
  (reverse1 ls '()))

(define (list-tail ls k)
  (if (zero? k)
      ls
      (list-tail (cdr ls) (1- k))))

(define (list-ref ls k)
  (car (list-tail ls k)))

(define (memq obj ls)
  (if (null? ls)
      #f
      (or (eq? obj (car ls))
          (memq obj (cdr ls)))))

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
         (error :arg-error :msg "proper-length: argument not a list"))
        ((null? (cdr ls))
         1)
        ((not (pair? (cdr ls)))
         1)
        (#t (1+ (proper-length (cdr ls))))))

(define (is-improper ls)
  (if (null? ls)
      #t
      (if (null? (cdr ls))
          #t
          (is-improper (cdr ls)))))

;; general comparison

(define (eqv? x y)
  ;; for our current implementation, eq? already does the same thing as eqv?
  ;; should
  (eq? x y))

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

(define (string=?1 s1 s2)
  (and (eqv? (string-length s1) (string-length s2))
       (all? (map char=? (string->list s1) (string->list s2)))))

(define (string=? . strings)
  (if (null? (cdr strings))
      #t
      (all? (pairwise string=?1 strings))))

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
