(declare (unit parse-utils))

(define-record token type text)
(define token make-token)

(define-record syntree type text children)
(define syntree make-syntree)

(define (char-match buf expect)
  (define actual (buf-lookahead! buf 1))
  (if (eof-object? actual)
    (error (string-append "Expected '" (string expect) "', received EOF instead"))
    (if (equal? expect actual)
      (buf-consume! buf)
      (error
        (string-append
          "Expected '" (string expect) "', received '" (string actual) "' instead"))))
  actual)

(define (token-match buf expect)
  (define actual (buf-lookahead! buf 1))
  (if (equal? expect (token-type actual))
    (buf-consume! buf)
    (error
      (string-append
        "Expected a " expect ", received a " actual " instead")))
  actual)

