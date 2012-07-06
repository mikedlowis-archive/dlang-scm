(declare (unit lex))

(define-record token type text)

(define token make-token)

(define (match buf expect)
  (define actual (buf-lookahead! buf 1))
  (if (equal? expect actual)
    (buf-consume! buf)
    (error
      (string-append
        "Expected '" expect "', received '" actual "'")))
  actual)

