(declare (unit parser)
         (uses buf))

(define (dlang/program in)
  (define result '())
  (while (not (eof-object? (buf-lookahead! in 1)))
    (append result (list (dlang/expression in)))))

(define (dlang/expression in)
  (if (core-form? in)
    (core-from in)
    (let ((result (basic-expr in))
          (ret    '()))
      (if (equal? 'lpar (buf-lookahead! in 1))
        (begin
          (match in 'lpar)
          (set! ret (expr-list in))
          (match in 'rpar)))
      ret)))

(define (dlang/core-form in) '())

(define (dlang/basic-expr in) '())

(define (dlang/literal in) '())

(define (dlang/expr-list in) '())

(define (dlang/id-list in) '())

