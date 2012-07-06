; For loop macro
(define-syntax for
  (syntax-rules ()
    ((_ var lst body ...)
      (let loop ((var (car lst)))
        body ...
        (if (< var (cadr lst))
          (loop (+ var (caddr lst))))))))

; While loop macro
(define-syntax while
  (syntax-rules ()
    ((_ cnd body ...)
      (let loop ()
        body ...
        (if cnd (loop))))))

; Until loop macro
(define-syntax until
  (syntax-rules ()
    ((_ cnd body ...)
      (let loop ()
        body ...
        (if (not cnd) (loop))))))

