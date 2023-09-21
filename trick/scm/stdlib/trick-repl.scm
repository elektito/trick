(define-syntax import
  (syntax-rules ()
    ((_ import-set ...)
     (begin
       (#$/compile/imp (interaction-environment) 'import-set) ...
       (void)))))
