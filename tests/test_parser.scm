(include "test.scm")
(declare (unit test_parser)
         (uses parser lexer buf))

; Helper functions
;------------------------------------------------------------------------------
(define (make-lexer input)
  (buf (buf input read-char) dlang/tokenize))

; dlang/operator-app
;------------------------------------------------------------------------------

; dlang/operator
;------------------------------------------------------------------------------
(def-test "dlang/operator should parse an Id"
  (call-with-input-string "abc"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/operator lxr))
      (and (syntree? result)
           (equal? 'id (syntree-type result))
           (equal? "abc" (syntree-text result))
           (equal? '() (syntree-children result))))))

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
      (and (syntree? result)
           (equal? 'id (syntree-type result))
           (equal? "abc" (syntree-text result))
           (equal? '() (syntree-children result))
           (eof-object? (buf-lookahead! lxr 1))))))

(def-test "dlang/literal should parse a Character"
  (call-with-input-string "'a'"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/literal lxr))
      (and (syntree? result)
           (equal? 'character (syntree-type result))
           (equal? "'a'" (syntree-text result))
           (equal? '() (syntree-children result))))))

(def-test "dlang/literal should parse a String"
  (call-with-input-string "\"abc\""
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/literal lxr))
      (and (syntree? result)
           (equal? 'string (syntree-type result))
           (equal? "\"abc\"" (syntree-text result))
           (equal? '() (syntree-children result))))))

(def-test "dlang/literal should parse a Symbol"
  (call-with-input-string "$abc"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/literal lxr))
      (and (syntree? result)
           (equal? 'symbol (syntree-type result))
           (equal? "$abc" (syntree-text result))
           (equal? '() (syntree-children result))))))

(def-test "dlang/literal should parse a Number"
  (call-with-input-string "1.0"
    (lambda (input)
      (define lxr (make-lexer input))
      (define result (dlang/literal lxr))
      (and (syntree? result)
           (equal? 'number (syntree-type result))
           (equal? "1.0" (syntree-text result))
           (equal? '() (syntree-children result))))))

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

