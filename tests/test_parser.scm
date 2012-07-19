(include "test.scm")
(declare (unit test_parser)
         (uses parser lexer buf))

; Helper functions
;------------------------------------------------------------------------------
(define (make-lexer input)
  (buf (buf input read-char) dlang/tokenize))

; dlang/program
;------------------------------------------------------------------------------

; dlang/expression
;------------------------------------------------------------------------------

; dlang/core-form
;------------------------------------------------------------------------------
(def-test "dlang/define should parse a variable definition"
  (call-with-input-string "def foo 1.0;"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/core-form lxr))
      (syntree=? result
        (syntree 'define ""
          (list
            (syntree 'id "foo" '())
            (syntree 'number "1.0" '())))))))

(def-test "dlang/core-form should parse a variable assignment"
  (call-with-input-string "set! foo 1.0;"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/core-form lxr))
      (syntree=? result
        (syntree 'assign ""
          (list
            (syntree 'id "foo" '())
            (syntree 'number "1.0" '())))))))

(def-test "dlang/core-form should parse an if statement"
  (call-with-input-string "if cond result1;"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/core-form lxr))
      (syntree=? result
        (syntree 'if ""
          (list
            (syntree 'id "cond" '())
            (syntree 'id "result1" '())))))))

(def-test "dlang/core-form should parse a begin block"
  (call-with-input-string "begin;"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/core-form lxr))
      (syntree=? result
        (syntree 'begin "" '())))))

(def-test "dlang/core-form should parse a func"
  (call-with-input-string "func();"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/core-form lxr))
      (syntree=? result
        (syntree 'func ""
          (list
            (syntree 'args "" '())
            (syntree 'block "" '())))))))

; dlang/core-form?
;------------------------------------------------------------------------------
(def-test "dlang/core-form? should recognize def as a core form"
  (call-with-input-string "def"
    (lambda (input)
      (define lxr (make-lexer input))
      (equal? #t (dlang/core-form? lxr)))))

(def-test "dlang/core-form? should recognize set! as a core form"
  (call-with-input-string "set!"
    (lambda (input)
      (define lxr (make-lexer input))
      (equal? #t (dlang/core-form? lxr)))))

(def-test "dlang/core-form? should recognize def as a core form"
  (call-with-input-string "if"
    (lambda (input)
      (define lxr (make-lexer input))
      (equal? #t (dlang/core-form? lxr)))))

(def-test "dlang/core-form? should recognize def as a core form"
  (call-with-input-string "begin"
    (lambda (input)
      (define lxr (make-lexer input))
      (equal? #t (dlang/core-form? lxr)))))

(def-test "dlang/core-form? should recognize def as a core form"
  (call-with-input-string "func"
    (lambda (input)
      (define lxr (make-lexer input))
      (equal? #t (dlang/core-form? lxr)))))

(def-test "dlang/core-form? should return false for non-coreform"
  (call-with-input-string "foo"
    (lambda (input)
      (define lxr (make-lexer input))
      (equal? #f (dlang/core-form? lxr)))))

; dlang/define
;------------------------------------------------------------------------------
(def-test "dlang/define should parse a variable definition"
  (call-with-input-string "def foo 1.0;"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/define lxr))
      (syntree=? result
        (syntree 'define ""
          (list
            (syntree 'id "foo" '())
            (syntree 'number "1.0" '())))))))

; dlang/assign
;------------------------------------------------------------------------------
(def-test "dlang/assign should parse a variable assignment"
  (call-with-input-string "set! foo 1.0;"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/assign lxr))
      (syntree=? result
        (syntree 'assign ""
          (list
            (syntree 'id "foo" '())
            (syntree 'number "1.0" '())))))))

; dlang/if
;------------------------------------------------------------------------------
(def-test "dlang/if should parse an if statement with one branch"
  (call-with-input-string "if cond result1;"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/if lxr))
      (syntree=? result
        (syntree 'if ""
          (list
            (syntree 'id "cond" '())
            (syntree 'id "result1" '())))))))

(def-test "dlang/if should parse an if statement with two branches"
  (call-with-input-string "if cond result1 result2;"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/if lxr))
      (syntree=? result
        (syntree 'if ""
          (list
            (syntree 'id "cond" '())
            (syntree 'id "result1" '())
            (syntree 'id "result2" '())))))))

; dlang/begin
;------------------------------------------------------------------------------
(def-test "dlang/begin should parse a begin block with 0 expressions"
  (call-with-input-string "begin;"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/begin lxr))
      (syntree=? result
        (syntree 'begin "" '())))))

(def-test "dlang/begin should parse a begin block with 1 expression"
  (call-with-input-string "begin stm1;"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/begin lxr))
      (syntree=? result
        (syntree 'begin ""
          (list
            (syntree 'id "stm1" '())))))))

(def-test "dlang/begin should parse a begin block with 2 expressions"
  (call-with-input-string "begin stm1 stm2;"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/begin lxr))
      (syntree=? result
        (syntree 'begin ""
          (list
            (syntree 'id "stm1" '())
            (syntree 'id "stm2" '())))))))

; dlang/func
;------------------------------------------------------------------------------
(def-test "dlang/func should parse an empty func"
  (call-with-input-string "func();"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/func lxr))
      (syntree=? result
        (syntree 'func ""
          (list
            (syntree 'args "" '())
            (syntree 'block "" '())))))))

(def-test "dlang/func should parse a func with one statement in the body"
  (call-with-input-string "func() stm1;"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/func lxr))
      (syntree=? result
        (syntree 'func ""
          (list
            (syntree 'args "" '())
            (syntree 'block ""
              (list
                (syntree 'id "stm1" '())))))))))

(def-test "dlang/func should parse a func with two statements in the body"
  (call-with-input-string "func() stm1 stm2;"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/func lxr))
      (syntree=? result
        (syntree 'func ""
          (list
            (syntree 'args "" '())
            (syntree 'block ""
              (list
                (syntree 'id "stm1" '())
                (syntree 'id "stm2" '())))))))))

(def-test "dlang/func should parse a func with one param"
  (call-with-input-string "func(a);"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/func lxr))
      (syntree=? result
        (syntree 'func ""
          (list
            (syntree 'args ""
              (list
                (syntree 'id "a" '())
                ))
            (syntree 'block "" '())))))))

(def-test "dlang/func should parse a func with two params"
  (call-with-input-string "func(a,b);"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/func lxr))
      (syntree=? result
        (syntree 'func ""
          (list
            (syntree 'args ""
              (list
                (syntree 'id "a" '())
                (syntree 'id "b" '())))
            (syntree 'block "" '())))))))

; dlang/basic-expr
;------------------------------------------------------------------------------
(def-test "dlang/basic-expr should parse a literal"
  (call-with-input-string "abc"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/basic-expr lxr))
      (syntree=? result (syntree 'id "abc" '())))))

(def-test "dlang/basic-expr should parse an infix operator application"
  (call-with-input-string "(1.0 * 2.0)"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/basic-expr lxr))
      (syntree=? result
        (syntree 'apply ""
          (list
            (syntree 'id "*" '())
            (syntree 'number "1.0" '())
            (syntree 'number "2.0" '())))))))

; dlang/operator-app
;------------------------------------------------------------------------------
(def-test "dlang/operator-app should parse an infix operator application"
  (call-with-input-string "(1.0 * 2.0)"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/operator-app lxr))
      (syntree=? result
        (syntree 'apply ""
          (list
            (syntree 'id "*" '())
            (syntree 'number "1.0" '())
            (syntree 'number "2.0" '())))))))

(def-test "dlang/operator-app should error when first paren missing"
  (call-with-input-string "1.0 * 2.0)"
    (lambda (input)
      (define lxr (make-lexer input))
      (check-exception
        "Expected a token of type 'lpar, received 'number instead"
        (dlang/operator-app lxr)))))

(def-test "dlang/operator-app should error when second paren missing"
  (call-with-input-string "(1.0 * 2.0"
    (lambda (input)
      (define lxr (make-lexer input))
      (check-exception "Expected a token of type 'rpar, received EOF instead"
        (dlang/operator-app lxr)))))

(def-test "dlang/operator-app should error operator is not an id"
  (call-with-input-string "(1.0 2.0 3.0)"
    (lambda (input)
      (define lxr (make-lexer input))
      (check-exception "Expected a token of type 'id, received 'number instead"
        (dlang/operator-app lxr)))))

; dlang/operator
;------------------------------------------------------------------------------
(def-test "dlang/operator should parse an Id"
  (call-with-input-string "abc"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/operator lxr))
      (syntree=? result (syntree 'id "abc" '())))))

(def-test "dlang/operator should error if not an Id"
  (call-with-input-string "1.0"
    (lambda (input)
      (define lxr (make-lexer input))
     (check-exception "Expected a token of type 'id, received 'number instead"
        (dlang/operator lxr)))))

(def-test "dlang/operator should error if EOF"
  (call-with-input-string ""
    (lambda (input)
      (define lxr (make-lexer input))
      (check-exception "Expected a token of type 'id, received EOF instead"
        (dlang/operator lxr)))))

; dlang/literal
;------------------------------------------------------------------------------
(def-test "dlang/literal should parse an Id"
  (call-with-input-string "abc"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/literal lxr))
      (syntree=? result (syntree 'id "abc" '())))))

(def-test "dlang/literal should parse a Character"
  (call-with-input-string "'a'"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/literal lxr))
      (syntree=? result (syntree 'character "'a'" '())))))

(def-test "dlang/literal should parse a String"
  (call-with-input-string "\"abc\""
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/literal lxr))
      (syntree=? result (syntree 'string "\"abc\"" '())))))

(def-test "dlang/literal should parse a Symbol"
  (call-with-input-string "$abc"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/literal lxr))
      (syntree=? result (syntree 'symbol "$abc" '())))))

(def-test "dlang/literal should parse a Number"
  (call-with-input-string "1.0"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/literal lxr))
      (syntree=? result (syntree 'number "1.0" '())))))

(def-test "dlang/literal should error when no literal found"
  (call-with-input-string "("
    (lambda (input)
      (define lxr (make-lexer input))
      (check-exception "Expected a literal"
        (dlang/literal lxr)))))

(def-test "dlang/literal should error when EOF"
  (call-with-input-string ""
    (lambda (input)
      (define lxr (make-lexer input))
      (check-exception "Expected a literal"
        (dlang/literal lxr)))))

; dlang/arg-list
;------------------------------------------------------------------------------
(def-test "dlang/arg-list should recognize an empty id list"
  (call-with-input-string "()"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/arg-list lxr))
      (syntree=? result (syntree 'arglist "" '())))))

(def-test "dlang/arg-list should recognize an arg list of length 1"
  (call-with-input-string "(a)"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/arg-list lxr))
      (syntree=? result
        (syntree 'arglist ""
          (list (syntree 'id "a" '())))))))

(def-test "dlang/arg-list should recognize an arg list of length 2"
  (call-with-input-string "(a,1.0)"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/arg-list lxr))
      (syntree=? result
        (syntree 'arglist ""
          (list
            (syntree 'id "a" '())
            (syntree 'number "1.0" '())))))))

(def-test "dlang/arg-list should recognize an arg list of length 3"
  (call-with-input-string "(a,1.0,$c)"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/arg-list lxr))
      (syntree=? result
        (syntree 'arglist ""
          (list
            (syntree 'id "a" '())
            (syntree 'number "1.0" '())
            (syntree 'symbol "$c" '())))))))

(def-test "dlang/arg-list should error when first paren missing"
  (call-with-input-string ")"
    (lambda (input)
      (define lxr (make-lexer input))
      (check-exception "Expected a token of type 'lpar, received 'rpar instead"
        (dlang/arg-list lxr)))))

(def-test "dlang/arg-list should error when second paren missing"
  (call-with-input-string "("
    (lambda (input)
      (define lxr (make-lexer input))
      (check-exception "Expected a literal"
        (dlang/arg-list lxr)))))

(def-test "dlang/arg-list should error when comma missing between args"
  (call-with-input-string "(a b)"
    (lambda (input)
      (define lxr (make-lexer input))
      (check-exception "Expected a token of type 'comma, received 'id instead"
        (dlang/arg-list lxr)))))

; dlang/arg-list?
;------------------------------------------------------------------------------
(def-test "dlang/arg-list? should return true if input contains an arg list"
  (call-with-input-string "(a, 1.0, $c)"
    (lambda (input)
      (define lxr (make-lexer input))
      (equal? #t (dlang/arg-list? lxr)))))

(def-test "dlang/arg-list? should return false if input does not contain an arg list"
  (call-with-input-string "(a b c)"
    (lambda (input)
      (define lxr (make-lexer input))
      (equal? #f (dlang/arg-list? lxr)))))

; dlang/id-list
;------------------------------------------------------------------------------
(def-test "dlang/id-list should recognize an empty id list"
  (call-with-input-string "()"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/id-list lxr))
      (syntree=? result (syntree 'args "" '())))))

(def-test "dlang/id-list should recognize an id list of length 1"
  (call-with-input-string "(a)"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/id-list lxr))
      (syntree=? result
        (syntree 'args ""
          (list (syntree 'id "a" '())))))))

(def-test "dlang/id-list should recognize an id list of length 2"
  (call-with-input-string "(a,b)"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/id-list lxr))
      (syntree=? result
        (syntree 'args ""
          (list
            (syntree 'id "a" '())
            (syntree 'id "b" '())))))))

(def-test "dlang/id-list should recognize an id list of length 3"
  (call-with-input-string "(a,b,c)"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/id-list lxr))
      (syntree=? result
        (syntree 'args ""
          (list
            (syntree 'id "a" '())
            (syntree 'id "b" '())
            (syntree 'id "c" '())))))))

(def-test "dlang/id-list should error when non-id recieved"
  (call-with-input-string "(1.0)"
    (lambda (input)
      (define lxr (make-lexer input))
      (check-exception "Expected a token of type 'id, received 'number instead"
        (dlang/id-list lxr)))))

(def-test "dlang/id-list should error when no comma in between ids"
  (call-with-input-string "(a b)"
    (lambda (input)
      (define lxr (make-lexer input))
      (check-exception "Expected a token of type 'comma, received 'id instead"
        (dlang/id-list lxr)))))

(def-test "dlang/id-list should error when left paren missing"
  (call-with-input-string ")"
    (lambda (input)
      (define lxr (make-lexer input))
      (check-exception "Expected a token of type 'lpar, received 'rpar instead"
        (dlang/id-list lxr)))))

(def-test "dlang/id-list should error when right paren missing"
  (call-with-input-string "("
    (lambda (input)
      (define lxr (make-lexer input))
      (check-exception "Expected a token of type 'id, received EOF instead"
        (dlang/id-list lxr)))))

; dlang/expr-block
;------------------------------------------------------------------------------
(def-test "dlang/expr-block should parse a block of 0 expressions"
  (call-with-input-string ";"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/expr-block lxr 'term))
      (syntree=? result (syntree 'block "" '())))))

(def-test "dlang/expr-block should parse a block of 1 expression"
  (call-with-input-string "1.0;"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/expr-block lxr 'term))
      (syntree=? result
        (syntree 'block ""
          (list
            (syntree 'number "1.0" '())))))))

(def-test "dlang/expr-block should parse a block of 2 expressions"
  (call-with-input-string "1.0 2.0;"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/expr-block lxr 'term))
      (syntree=? result
        (syntree 'block ""
          (list
            (syntree 'number "1.0" '())
            (syntree 'number "2.0" '())))))))

(def-test "dlang/expr-block should error when no terminator found"
  (call-with-input-string "1.0 2.0"
    (lambda (input)
      (define lxr (make-lexer input))
      (check-exception "Expected a literal"
        (dlang/expr-block lxr 'term)))))

