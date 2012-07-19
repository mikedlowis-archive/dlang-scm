(declare (unit parse-utils))

(define-record token type text)
(define token make-token)

(define-record syntree type text children)
(define syntree make-syntree)

(define (syntree=? tr1 tr2)
  (and (equal? (syntree-type tr1) (syntree-type tr2))
       (equal? (syntree-text tr1) (syntree-text tr2))
       (syntree-children=? (syntree-children tr1) (syntree-children tr2))))

(define (syntree-children=? ch1 ch2)
  (and
    (or
      (and (null? ch1) (null? ch2))
      (and (not (null? ch1)) (not (null? ch2))))
    (if (null? ch1)
      #t ; If we got here and one is null then BOTH must be, hence equal
      (and
        (syntree=? (car ch1) (car ch2))
        (syntree-children=? (cdr ch1) (cdr ch2))))))

(define (char-match buf expect)
  (define actual (buf-lookahead! buf 1))
  (if (eof-object? actual)
    (abort (string-append "Expected '" (string expect) "', received EOF instead"))
    (if (equal? expect actual)
      (buf-consume! buf)
      (abort
        (string-append
          "Expected '" (string expect) "', received '" (string actual) "' instead"))))
  actual)

(define (token-match buf expect)
  (define actual (buf-lookahead! buf 1))
  (if (eof-object? actual)
    (abort
      (string-append
        "Expected a token of type '" (symbol->string expect) ","
        " received EOF instead"))
    (if (equal? expect (token-type actual))
      (buf-consume! buf)
      (abort
        (string-append
          "Expected a token of type '" (symbol->string expect) ","
          " received '" (symbol->string (token-type actual)) " instead"))))
  actual)

(define (token-matches? buf expect)
  (define actual (buf-lookahead! buf 1))
  (and (not (eof-object? actual))
       (equal? expect (token-type actual))))

(define (keyword-match buf expect)
  (define actual (buf-lookahead! buf 1))
  (if (and (token-matches? buf 'id)
           (equal? expect (token-text actual)))
    (buf-consume! buf)
    (abort
      (string-append
        "Expected '" expect "', received '" (token-text actual) "' instead"))))

(define (token->syntree tok)
  (syntree (token-type tok) (token-text tok) '()))

(define (test-apply fn buf . args)
  (define result '())
  (buf-mark! buf)
  (call/cc
    (lambda (cont)
      (with-exception-handler
        (lambda (x) (cont '()))
        (lambda ()  (set! result (apply fn (append buf args)))))))
  (buf-release! buf)
  (not (null? result)))
