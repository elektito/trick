;; allow some primitives to be used like normal functions

(define (cons x y) (cons x y))
(define (car x) (car x))
(define (cdr x) (cdr x))
(define (iadd x y) (iadd x y))
(define (isub x y) (isub x y))
(define (imul x y) (imul x y))
(define (idiv x y) (idiv x y))
(define (shr x y) (shr x y))
(define (shl x y) (shl x y))
(define (asr x y) (asr x y))
(define (b-not x) (b-not x))
(define (b-and x y) (b-and x y))
(define (b-or x y) (b-or x y))
(define (b-xor x y) (b-xor x y))
(define (< x y) (< x y))
(define (<= x y) (<= x y))
(define (print x) (print x))
(define (printc c) (print c))
(define (halt n) (halt n))
(define (type x) (type x))
(define (eq? x y) (eq? x y))
(define (gensym ) (gensym))

;; type predicates

(define (null? x) (eq? 'null (type x)))

(define (atom? v)
  ;; everything besides cons (3) is an atom
  (if (eq? (type v) 'list) #f #t))

(define (symbol? v) (eq? (type v) 'symbol))
(define (list? v) (eq? (type v) 'list))
(define (int? v) (eq? (type v) 'int))
(define (string? v) (eq? (type v) 'string))
(define (closure? v) (eq? (type v) 'closure))
(define (bool? v) (eq? (type v) 'bool))

;; list utilities

(define (list & values) values)

(define (concat2 l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (concat2 (cdr l1) l2))))

(define (concat1 lists)
  (if (null? lists)
      nil
      (concat2 (car lists)
               (concat1 (cdr lists)))))

(define (concat & lists)
  (concat1 lists))

(define (last x)
  (if (null? x)
      nil
      (if (null? (cdr x))
          (car x)
          (last (cdr x)))))

(define (butlast x)
  (if (null? x)
      nil
      (if (null? (cdr x))
          nil
          (cons (car x) (butlast (cdr x))))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (mapcar func args)
  (if (null? args)
      '()
      (cons (func (car args))
            (mapcar func (cdr args)))))

;; begin and cond

(define-macro (begin & body)
  (list (concat (list 'lambda nil) body)))

(define-macro (cond & arms)
  (if (null? arms)
      nil
      (list 'if
            (caar arms)
            (cons 'begin (cdar arms))
            (cons 'cond (cdr arms)))))

;; arithmetic

(define (+ & r)
  (if (null? r)
      0
      (iadd (car r) (apply + (cdr r)))))

(define (- & r)
  (if (null? r)
      (error :arg-error :msg "Invalid number of arguments for -")
      (if (null? (cdr r))
          (isub 0 (car r))
          (if (null? (cddr r))
              (isub (car r) (cadr r))
              (isub (apply - (butlast r)) (last r))))))

(define (* & r)
  (if (null? r)
      1
      (imul (car r) (apply * (cdr r)))))

(define (/ & r)
  (if (null? r)
      (error :arg-error :msg "Invalid number of arguments for /")
      (if (null? (cdr r))
          (idiv 1 (car r))
          (if (null? (cddr r))
              (idiv (car r) (cadr r))
              (idiv (apply / (butlast r)) (last r))))))

(define (remainder a b)
  (irem a b))

(define (modulo a b)
  (let ((res (remainder a b)))
    (if (< b 0)
        (if (<= res 0) res (+ res b))
        (if (>= res 0) res (+ res b)))))

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
         (if (eq? level 1)
             (list 'quote form)
             form))
        ((bq-is-unquote form)
         (if (eq? level 1)
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

;; comparison

(define (> x y) (not (<= x y)))
(define (>= x y) (not (< x y)))
(define (zero? x) (eq? x 0))
(define (negative? x) (< x 0))
(define (positive? x) (> x 0))

;; more macros now that we have backquote!

(define-macro (with-gensyms names & body)
  `(let ,(mapcar (lambda (name) `(,name (gensym))) names)
     ,@body))

(define-macro (and & forms)
  (cond ((null? forms) '#t)
        ((null? (cdr forms)) (car forms))
        (#t `(if ,(car forms)
                 (and ,@(cdr forms))
                 '#f))))

(define-macro (or & forms)
  (cond ((null? forms) '#f)
        ((null? (cdr forms)) (car forms))
        (#t (with-gensyms (xcar)
              `(let ((,xcar ,(car forms)))
                 (if ,xcar ,xcar (or ,@(cdr forms))))))))

(define-macro (let* bindings & body)
  (if (null? bindings)
      `(begin ,@body)
      `(let (,(car bindings))
         (let* ,(cdr bindings) ,@body))))

(define (not x)
  (if x #f #t))

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

(define (map1 func arg-lists)
  (if (or (null? arg-lists)
          (any? (mapcar null? arg-lists)))
      nil
      (cons (apply func (mapcar car arg-lists))
            (map1 func (mapcar cdr arg-lists)))))

(define (map func & arg-lists)
  (map1 func arg-lists))

;; generalized/recursive comparison

;; compare two lists recursively using eq?
(define (list-eq? list1 list2)
  (cond ((null? list1) (null? list2))
        ((null? list2) (null? list1))
        ((not (eq? (type list1) 'list))
         #f)
        ((not (eq? (type list2) 'list))
         #f)
        ((and (eq? (type (car list1)) 'list)
              (eq? (type (car list2)) 'list))
         (and (list-eq? (car list1) (car list2))
              (list-eq? (cdr list1) (cdr list2))))
        (#t (and (eq? (car list1) (car list2))
                 (list-eq? (cdr list1) (cdr list2))))))

(define (=' v1 v2)
  (cond ((not (eq? (type v1) (type v2)))
         #f)
        ((eq? (type v1) 'list)
         (list-eq? v1 v2))
        (#t (eq? v1 v2))))

(define (= & r)
  (cond ((null? r) ; no arguments
         (error :arg-error :msg "Invalid number of arguments for ="))
        ((null? (cdr r)) ; one argument
         #t)
        ((null? (cddr r)) ; two arguments
         (=' (car r) (cadr r)))
        (#t ; more than two arguments
         (and (=' (car r) (cadr r))
              (apply = (cdr r))))))
