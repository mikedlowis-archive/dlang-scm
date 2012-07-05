
(define-syntax def-test
  (syntax-rules ()
    ((_ desc body ...)
      (register-test!
        (cons desc
          (lambda () body ...))))))

