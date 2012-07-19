(include "loop.scm")
(declare (unit parser)
         (uses buf))

;------------------------------------------------------------------------------
; Formal EBNF Syntax:
;
; Program := Expression*
;
; Expression := CoreForm
;             | BasicExpr (ArgList)?
;
; CoreForm := 'def' ID Expression TERM
;           | 'set!' ID Expression TERM
;           | 'if' Expression Expression (Expression)? TERM
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
; ExpBlock := (Expression)*
;------------------------------------------------------------------------------

(define (dlang/program in)
  (define result '())
  (while (not (eof-object? (buf-lookahead! in 1)))
    (append result (list (dlang/expression in)))))

(define (dlang/expression in)
  (if (dlang/core-form? in)
    (dlang/core-form in)
    (let ((result (dlang/basic-expr in)))
      (if (dlang/arg-list? in) (dlang/arg-list? in result) result))))

(define (dlang/core-form in)
  (define tok (buf-lookahead! in 1))
  (define text (if (eof-object? tok) "" (token-text tok)))
  (cond
    ((string=? text "def")   (dlang/define in))
    ((string=? text "set!")  (dlang/assign in))
    ((string=? text "if")    (dlang/if in))
    ((string=? text "begin") (dlang/begin in))
    ((string=? text "func")  (dlang/func in))
    (else (error "Expected a core form"))))

(define (dlang/core-form? in)
  (define tok (buf-lookahead! in 1))
  (define text (if (eof-object? tok) "" (token-text tok)))
  (or (string=? text "def")
      (string=? text "set!")
      (string=? text "if")
      (string=? text "begin")
      (string=? text "func")))

(define (dlang/define in)
  (define node '())
  (keyword-match in "def")
  (set! node
    (syntree 'define ""
      (list (token->syntree (token-match in 'id)) (dlang/expression in))))
  (token-match in 'term)
  node)

(define (dlang/assign in)
  (define node '())
  (keyword-match in "set!")
  (set! node
    (syntree 'assign ""
      (list (token->syntree (token-match in 'id)) (dlang/expression in))))
  (token-match in 'term)
  node)

(define (dlang/if in)
  (define node '())
  (keyword-match in "if")
  (set! node
    (syntree 'if "" (list (dlang/expression in) (dlang/expression in))))
  (if (not (token-matches? in 'term))
    (syntree-children-set! node
      (append (syntree-children node) (list (dlang/expression in)))))
  (token-match in 'term)
  node)

(define (dlang/begin in)
  (define node '())
  (keyword-match in "begin")
  (set! node (dlang/expr-block in 'term))
  (token-match in 'term)
  (syntree-type-set! node 'begin)
  node)

(define (dlang/func in)
  (define node (syntree 'func "" '()))
  (keyword-match in "func")
  (syntree-children-set! node
    (list (dlang/id-list in) (dlang/expr-block in 'term)))
  (token-match in 'term)
  (syntree-type-set! node 'func)
  node)

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
    (set! parts (append (list (dlang/expression in)) parts))
    (set! parts (append (list (dlang/operator in)) parts))
    (set! parts (append parts (list (dlang/expression in))))
    (token-match in 'rpar)
    (syntree-children-set! tree parts)
    tree))

(define (dlang/operator in)
  (define tok (token-match in 'id))
  (syntree (token-type tok) (token-text tok) '()))

(define (dlang/literal in)
  (define tok (buf-lookahead! in 1))
  (define type (if (eof-object? tok) '() (token-type tok)))
  (if (or (equal? 'id type)
          (equal? 'character type)
          (equal? 'string type)
          (equal? 'symbol type)
          (equal? 'number type))
    (set! tok (buf-consume! in))
    (abort "Expected a literal"))
  (syntree (token-type tok) (token-text tok) '()))

(define (dlang/arg-list in) '())

(define (dlang/arg-list? in)
  (test-apply dlang/arg-list in))

(define (dlang/id-list in)
  (define tree (syntree 'args "" '()))
  (define chldrn '())
  (token-match in 'lpar)
  (if (not (token-matches? in 'rpar))
    (begin
      (set! chldrn
        (append chldrn (list (token->syntree (token-match in 'id)))))
      (while (not (token-matches? in 'rpar))
        (token-match in 'comma)
        (set! chldrn
          (append chldrn (list (token->syntree (token-match in 'id))))))))
  (token-match in 'rpar)
  (syntree-children-set! tree chldrn)
  tree)

(define (dlang/expr-block in term)
  (define tree (syntree 'block "" '()))
  (define chldrn '())
  (while (not (token-matches? in term))
    (set! chldrn (append chldrn (list (dlang/expression in)))))
  (syntree-children-set! tree chldrn)
  tree)

