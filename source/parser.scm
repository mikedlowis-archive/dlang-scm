(declare (unit parser)
         (uses buf))

;------------------------------------------------------------------------------
; Formal EBNF Syntax:
;
; Program := Expression*
;
; Expression := CoreForm
;             | BasicExpr
;             | BasicExpr ArgList
;
; CoreForm := 'def' ID Expression TERM
;           | 'set!' ID Expression TERM
;           | 'if' Expression Expression Expression TERM
;           | 'begin' ExpBlock TERM
;           | 'func' IdList ExpBlock TERM
;
; BasicExpr := '(' Expression ID Expression ')'
;            | Literal
;
; Literal := ID | CHAR | STRING | SYMBOL | NUMBER
;
; ArgList := '(' Expression (',' Expression)* ')'
;
; IdList := '(' ID (',' ID)* ')'
;
; ExpBlock := Expression*
;------------------------------------------------------------------------------

(define (dlang/program in)
  (define result '())
  (while (not (eof-object? (buf-lookahead! in 1)))
    (append result (list (dlang/expression in)))))

(define (dlang/expression in)
  (if (core-form? in)
    (core-form in)
    (let ((result (dlang/basic-expr in))
          (ret    '()))
      (if (equal? 'lpar (buf-lookahead! in 1))
        (begin
          (match in 'lpar)
          (set! ret (dlang/expr-list in))
          (match in 'rpar)))
      ret)))

(define (dlang/core-form in) '())

(define (dlang/basic-expr in)
  (define tok (buf-lookahead! in 1))
  (if (equal? 'lpar (token-type tok))
    (dlang/operator-app in)
    (dlang/literal in)))

(define (dlang/operator-app in)
  (let ((tree (syntree 'apply "" '()))
        (parts '())
        (op '()))
    (token-match in 'lpar)
    (set! parts (cons (dlang/expression in)))
    (set! parts (cons (dlang/operator in)))
    (set! parts (append parts (list (dlang/expression in))))
    (token-match in 'rpar)
    (syntree-children-set! tree parts)
    tree))

(define (dlang/operator in)
  (define tok (buf-lookahead! in 1))
  (if (equal? 'id (token-type tok))
    (syntree (token-type tok) (token-text tok) '())
    (error "Expected an Id or operator.")))

(define (dlang/literal in)
  (define tok (buf-lookahead! in 1))
  (if (or (equal? 'id tok)
          (equal? 'character tok)
          (equal? 'string tok)
          (equal? 'symbol tok)
          (equal? 'number tok))
    (set! tok (buf-consume! in))
    (error "Expected a literal"))
  (syntree (token-type tok) (token-text tok) '()))

(define (dlang/arg-list in) '())

(define (dlang/id-list in)
  (define tree (syntree 'list "" '()))
  (define chldrn '())
  (token-match in 'lpar)
  (while (equal? 'id (token-type (buf-lookahead! in 1)))
    (define tok (buf-consume! in))
    (set! tok (syntree (token-type tok) (token-text tok) '()))
    (set! chldrn (append chldrn (list tok))))
  (token-match in 'rpar)
  (syntree-children-set! tree chldrn)
  tree)

(define (dlang/expr-list in term)
  (define tree (syntree 'list "" '()))
  (define chldrn '())
  (while (equal? term (token-type (buf-lookahead! in 1)))
    (set! chldrn (append chldrn (list (dlang/expression in)))))
  (syntree-children-set! tree chldrn)
  tree)

