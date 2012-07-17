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
(def-test "dlang/expression should parse a literal"
  (call-with-input-string "abc"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/expression lxr))
      (syntree=? result (syntree 'id "abc" '())))))

; dlang/core-form
;------------------------------------------------------------------------------

; dlang/define
;------------------------------------------------------------------------------

; dlang/assign
;------------------------------------------------------------------------------

; dlang/if
;------------------------------------------------------------------------------

; dlang/begin
;------------------------------------------------------------------------------

; dlang/func
;------------------------------------------------------------------------------

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
     (check-error "Expected a token of type 'id, received 'number instead"
        (dlang/operator lxr)))))

(def-test "dlang/operator should error if EOF"
  (call-with-input-string ""
    (lambda (input)
      (define lxr (make-lexer input))
      (check-error "Expected a token of type 'id, received EOF instead"
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
      (check-error "Expected a literal"
        (dlang/literal lxr)))))

(def-test "dlang/literal should error when EOF"
  (call-with-input-string ""
    (lambda (input)
      (define lxr (make-lexer input))
      (check-error "Expected a literal"
        (dlang/literal lxr)))))

; dlang/arg-list
;------------------------------------------------------------------------------

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
      (check-error "Expected a token of type 'id, received 'number instead"
        (dlang/id-list lxr)))))

(def-test "dlang/id-list should error when no comma in between ids"
  (call-with-input-string "(a b)"
    (lambda (input)
      (define lxr (make-lexer input))
      (check-error "Expected a token of type 'comma, received 'id instead"
        (dlang/id-list lxr)))))

(def-test "dlang/id-list should error when left paren missing"
  (call-with-input-string ")"
    (lambda (input)
      (define lxr (make-lexer input))
      (check-error "Expected a token of type 'lpar, received 'rpar instead"
        (dlang/id-list lxr)))))

(def-test "dlang/id-list should error when right paren missing"
  (call-with-input-string "("
    (lambda (input)
      (define lxr (make-lexer input))
      (check-error "Expected a token of type 'id, received EOF instead"
        (dlang/id-list lxr)))))

; dlang/expr-block
;------------------------------------------------------------------------------


