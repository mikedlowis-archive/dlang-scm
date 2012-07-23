(declare (unit scheme) (uses parse-utils extras))

(define (obj->string obj)
  (with-output-to-string
    (lambda () (display obj))))

(define (scheme-program lst)
  (if (null? lst) '()
    (append
      (list (scheme-expression (car lst)))
      (scheme-program (cdr lst)))))

(define (scheme-expression expr)
  (define type (syntree-type expr))
  (cond
    ((equal? type 'id)        (scheme-id expr))
    ((equal? type 'character) (scheme-character expr))
    ((equal? type 'number)    (scheme-number expr))
    ((equal? type 'symbol)    (scheme-symbol expr))
    ((equal? type 'string)    (scheme-string expr))
    ((equal? type 'define)    (scheme-define expr))
    ((equal? type 'assign)    (scheme-assign expr))
    ((equal? type 'if)        (scheme-if expr))
    ((equal? type 'begin)     (scheme-begin expr))
    ((equal? type 'func)      (scheme-func expr))
    ((equal? type 'apply)     (scheme-apply expr))
    (else "unknown")))

(define (scheme-id expr)
  (syntree-text expr))

(define (scheme-character expr)
  (string-append "#\\" (string (string-ref (syntree-text expr) 1))))

(define (scheme-number expr)
  (syntree-text expr))

(define (scheme-symbol expr)
  (string-append "'" (substring (syntree-text expr) 1)))

(define (scheme-string expr)
  (syntree-text expr))

(define (scheme-define expr)
  (string-append
    "(define "
    (obj->string (scheme-expression (list-ref (syntree-children expr) 0)))
    " "
    (obj->string (scheme-expression (list-ref (syntree-children expr) 1)))
    ")"))

(define (scheme-assign expr)
  (string-append
    "(set! "
    (obj->string (scheme-expression (list-ref (syntree-children expr) 0)))
    " "
    (obj->string (scheme-expression (list-ref (syntree-children expr) 1)))
    ")"))

(define (scheme-if expr)
  (append '("if")
    (map scheme-expression (syntree-children expr))))

(define (scheme-begin expr)
  (append '("begin")
    (map scheme-expression (syntree-children expr))))

(define (scheme-func expr)
  (append '("lambda")
    (scheme-idlist (car (syntree-children expr)))
    (scheme-block (cadr (syntree-children expr)))))

(define (scheme-apply expr)
  (map scheme-expression (syntree-children expr)))

(define (scheme-idlist expr)
  (list (map scheme-expression (syntree-children expr))))

(define (scheme-block expr)
  (if (null? (syntree-children expr))
    '('())
    (map scheme-expression (syntree-children expr))))

