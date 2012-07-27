(include "test.scm")
(declare (unit test_parse_utils))

; Tests for function definitions
;------------------------------------------------------------------------------
(def-test "functions for token creation and usage should be created"
  (and (procedure? make-token)
       (procedure? token)
       (procedure? token-text)
       (procedure? token-type)))

(def-test "functions for syntree creation and usage should be created"
  (and (procedure? make-syntree)
       (procedure? syntree)
       (procedure? syntree-text)
       (procedure? syntree-type)
       (procedure? syntree-children)))

; token=?
;------------------------------------------------------------------------------
(def-test "token=? should return true if trees are equal"
  (token=?
    (token 'foo "")
    (token 'foo "")))

(def-test "token=? should return false if types differ"
  (not
    (token=?
      (token 'foo "")
      (token 'bar ""))))

(def-test "token=? should return false if text differs"
  (not
    (token=?
      (token 'foo "a")
      (token 'foo "b"))))

; syntree=?
;------------------------------------------------------------------------------
(def-test "syntree=? should return true if trees are equal"
  (syntree=?
    (syntree 'foo "" '())
    (syntree 'foo "" '())))

(def-test "syntree=? should return false if types differ"
  (not
    (syntree=?
      (syntree 'foo "" '())
      (syntree 'bar "" '()))))

(def-test "syntree=? should return false if text differs"
  (not
    (syntree=?
      (syntree 'foo "a" '())
      (syntree 'foo "b" '()))))

(def-test "syntree=? should return false if children differ"
  (not
    (syntree=?
      (syntree 'foo "" '())
      (syntree 'foo "" '(1)))))

; syntree-children=?
;------------------------------------------------------------------------------
(def-test "syntree=? should return true is lists and elements are equal"
  (syntree-children=?
    (list (syntree 'foo "bar" '()))
    (list (syntree 'foo "bar" '()))))

(def-test "syntree=? should return true if both lists are null"
  (syntree-children=? '() '()))

(def-test "syntree=? should return false if only one of the lists is null"
  (not (syntree-children=? '() '(1))))

(def-test "syntree=? should return false if elements differ"
  (not
    (syntree-children=?
      (list (syntree 'foo "" '()))
      (list (syntree 'bar "" '())))))

; char-match
;------------------------------------------------------------------------------
(def-test "char-match should consume and return char if the next char matches"
  (call-with-input-string "a"
    (lambda (input)
      (define buffer (buf input read-char))
      (and (equal? #\a (char-match buffer #\a))
           (eof-object? (buf-lookahead! buffer 1))))))

(def-test "char-match should error when EOF"
  (call-with-input-string ""
    (lambda (input)
      (define buffer (buf input read-char))
      (check-exception "Expected 'a', received EOF instead"
        (char-match buffer #\a)))))

(def-test "char-match should error when chars do not match"
  (call-with-input-string "b"
    (lambda (input)
      (define buffer (buf input read-char))
      (check-exception "Expected 'a', received 'b' instead"
        (char-match buffer #\a)))))

; token-match
;------------------------------------------------------------------------------
(def-test "token-match should consume and return token if type matches"
  (call-with-input-string "a"
    (lambda (input)
      (define buffer (dlang/lexer input))
      (token=?
        (token-match buffer 'id)
        (token 'id "a")))))

(def-test "token-match should error when EOF received"
  (call-with-input-string ""
    (lambda (input)
      (define buffer (dlang/lexer input))
      (check-exception "Expected a token of type 'id, received EOF instead"
        (token-match buffer 'id)))))

(def-test "token-match should error if type does not match"
  (call-with-input-string "1.0"
    (lambda (input)
      (define buffer (dlang/lexer input))
      (check-exception "Expected a token of type 'id, received 'number instead"
        (token-match buffer 'id)))))

; token-matches?
;------------------------------------------------------------------------------

; keyword-match
;------------------------------------------------------------------------------

; token->syntree
;------------------------------------------------------------------------------
(def-test "token->syntree should convert a token to a syntree"
  (syntree=?
    (token->syntree (token 'foo "bar"))
    (syntree 'foo "bar" '())))

; test-apply
;------------------------------------------------------------------------------

; collect-char
;------------------------------------------------------------------------------

; consume-all
;------------------------------------------------------------------------------

; collect
;------------------------------------------------------------------------------

