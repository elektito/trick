;; some initial definitions so we can have defun

(define list (lambda (& rest) rest))

(define null? (lambda (x) (eq? 'null (type x))))

(define concat2 (lambda (l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (concat2 (cdr l1) l2)))))

(define concat1 (lambda (lists)
  (if (null? lists)
      nil
      (concat2 (car lists)
               (concat1 (cdr lists))))))

(define concat (lambda (& lists)
  (concat1 lists)))

;; defun

(defmac defun (name args & body)
  (list 'define
        name
        (concat (list 'lambda args) body)))

;; some more definitions so we can have backquote

(defun caar (x) (car (car x)))
(defun cdar (x) (cdr (car x)))
(defun cadr (x) (car (cdr x)))
(defun cddr (x) (cdr (cdr x)))

(defun mapcar (func args)
  (if (null? args)
      '()
      (cons (func (car args))
            (mapcar func (cdr args)))))

(defmac begin (& body)
  (list (concat (list 'lambda nil) body)))

(defmac cond (& arms)
  (if (null? arms)
      nil
      (list 'if
            (caar arms)
            (cons 'begin (cdar arms))
            (cons 'cond (cdr arms)))))

(defun atom? (v)
  ;; everything besides cons (3) is an atom
  (if (eq? (type v) 'list) #f #t))

(defun last (x)
  (if (null? x)
      nil
      (if (null? (cdr x))
          (car x)
          (last (cdr x)))))

(defun butlast (x)
  (if (null? x)
      nil
      (if (null? (cdr x))
          nil
          (cons (car x) (butlast (cdr x))))))

(defun + (& r)
  (if (null? r)
      0
      (iadd (car r) (apply + (cdr r)))))

(defun - (& r)
  (if (null? r)
      (error :arg-error :msg "Invalid number of arguments for -")
      (if (null? (cdr r))
          (isub 0 (car r))
          (if (null? (cddr r))
              (isub (car r) (cadr r))
              (isub (apply - (butlast r)) (last r))))))

(defun * (& r)
  (if (null? r)
      1
      (imul (car r) (apply * (cdr r)))))

(defun / (& r)
  (if (null? r)
      (error :arg-error :msg "Invalid number of arguments for /")
      (if (null? (cdr r))
          (idiv 1 (car r))
          (if (null? (cddr r))
              (idiv (car r) (cadr r))
              (idiv (apply / (butlast r)) (last r))))))

(defun remainder (a b)
  (irem a b))

(defun modulo (a b)
  (let ((res (remainder a b)))
    (if (< b 0)
        (if (<= res 0) res (+ res b))
        (if (>= res 0) res (+ res b)))))

;; backquote

(defun bq-simplify (form)
  ;; if there's an concat in which all arguments are lists of size 1, convert it
  ;; to a "list" call:
  ;; (concat '(x) (list y) '(z)) => (list 'x y 'z)
  ;;
  ;; if there is a list call in which all forms are quoted, convert it
  ;; to a single quoted list:
  ;; (list 'x 'y 'z) => '(x y z)
  form)

(defun bq-is-unquote (form)
  (cond ((atom? form) #f)
        ((null? form) #f)
        ((eq? (car form) 'unquote) #t)
        (#t #f)))

(defun bq-is-unquote-splicing (form)
  (cond ((atom? form) #f)
        ((eq? (car form) 'unquote-splicing) #t)
        (#t #f)))

(defun bq-is-backquote (form)
  (cond ((atom? form) #f)
        ((null? form) #f)
        ((eq? (car form) 'backquote) #t)
        (#t #f)))

(defun bq-process-list-item (form level)
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

(defun bq-process-list (form level)
  (cons 'concat
        (mapcar (lambda (form)
                  (bq-process-list-item form level))
                form)))

(defun bq-process (form level)
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

(defmac backquote (form)
  (bq-simplify
   (bq-process form 1)))

;; allow some primitives to be used like normal functions

(defun cons (x y) (cons x y))
(defun car (x) (car x))
(defun cdr (x) (cdr x))
(defun iadd (x y) (iadd x y))
(defun isub (x y) (isub x y))
(defun imul (x y) (imul x y))
(defun idiv (x y) (idiv x y))
(defun shr (x y) (shr x y))
(defun shl (x y) (shl x y))
(defun asr (x y) (asr x y))
(defun b-not (x) (b-not x))
(defun b-and (x y) (b-and x y))
(defun b-or (x y) (b-or x y))
(defun b-xor (x y) (b-xor x y))
(defun < (x y) (< x y))
(defun <= (x y) (<= x y))
(defun print (x) (print x))
(defun printc (c) (print c))
(defun halt (n) (halt n))
(defun type (x) (type x))
(defun eq? (x y) (eq? x y))
(defun gensym () (gensym))

;; everything else

(defun > (x y) (not (<= x y)))
(defun >= (x y) (not (< x y)))
(defun zero? (x) (eq? x 0))

(defmac with-gensyms (names & body)
  `(let ,(mapcar (lambda (name) `(,name (gensym))) names)
     ,@body))

(defmac and (& forms)
  (cond ((null? forms) '#t)
        ((null? (cdr forms)) (car forms))
        (#t `(if ,(car forms)
                 (and ,@(cdr forms))
                 '#f))))

(defmac or (& forms)
  (cond ((null? forms) '#f)
        ((null? (cdr forms)) (car forms))
        (#t (with-gensyms (xcar)
              `(let ((,xcar ,(car forms)))
                 (if ,xcar ,xcar (or ,@(cdr forms))))))))

;;;;;;;;;;;;;;;;

(defun not (x)
  (if x #f #t))

(defun any? (values)
  (if (null? values)
      #f
      (if (car values)
          #t
          (any? (cdr values)))))

(defun all? (values)
  (if (null? values)
      #t
      (if (car values)
          (all? (cdr values))
          #f)))

(defun map1 (func arg-lists)
  (if (or (null? arg-lists)
          (any? (mapcar null? arg-lists)))
      nil
      (cons (apply func (mapcar car arg-lists))
            (map1 func (mapcar cdr arg-lists)))))

(defun map (func & arg-lists)
  (map1 func arg-lists))

;; compare two lists recursively using eq?
(defun list-eq? (list1 list2)
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

(defun =' (v1 v2)
  (cond ((not (eq? (type v1) (type v2)))
         #f)
        ((eq? (type v1) 'list)
         (list-eq? v1 v2))
        (#t (eq? v1 v2))))

(defun = (& r)
  (cond ((null? r) ; no arguments
         (error :arg-error :msg "Invalid number of arguments for ="))
        ((null? (cdr r)) ; one argument
         #t)
        ((null? (cddr r)) ; two arguments
         (=' (car r) (cadr r)))
        (#t ; more than two arguments
         (and (=' (car r) (cadr r))
              (apply = (cdr r))))))
