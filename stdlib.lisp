;; some initial definitions so we can have defun

(define list (lambda (& rest) rest))

(define null? (lambda (x) (eq? 1 (type x))))

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

(defun atom? (x)
  (cond ((null? x) #t)
        ((eq? (type x) 3) #f) ; cons
        (#t #t)))

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
        ((null? form) #f)
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

;; everything else

(defun > (x y) (< y x))
(defun = (x y)
  (cond ((< x y) #f)
        ((> x y) #f)
        (#t #t)))

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
