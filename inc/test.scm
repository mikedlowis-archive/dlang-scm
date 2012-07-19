
(define-syntax def-test
  (syntax-rules ()
    ((_ desc body ...)
      (register-test!
        (cons desc
          (lambda () body ...))))))

(define-syntax check-error
  (syntax-rules ()
    ((_ expect expr)
      (let ((prev error))
        (define result
          (call/cc
            (lambda (err)
              (set! error err)
              expr)))
        (set! error prev)
        (equal? expect result)))))

(define-syntax check-exception
  (syntax-rules ()
    ((_ expect expr)
      (equal? expect
        (call/cc
          (lambda (cont)
            (with-exception-handler
              (lambda (x) (cont x))
              (lambda ()  expr))))))))

