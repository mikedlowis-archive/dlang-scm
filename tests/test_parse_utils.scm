(include "test.scm")
(declare (unit test_parse_utils))

; Tests for function definitions
;------------------------------------------------------------------------------
(def-test "functions for token creation and usage should be created"
  (and (procedure? make-token)
       (procedure? token)
       (procedure? token-type)
       (procedure? token-text)
       (procedure? token-pos)))

(def-test "functions for syntree creation and usage should be created"
  (and (procedure? make-syntree)
       (procedure? syntree)
       (procedure? syntree-text)
       (procedure? syntree-type)
       (procedure? syntree-children)))

(def-test "functions for charport creation and usage should be created"
  (and (procedure? make-charport)
       (procedure? charport)
       (procedure? charport-port)
       (procedure? charport-line)
       (procedure? charport-column)))

(def-test "functions for posdata creation and usage should be created"
  (and (procedure? make-syntree)
       (procedure? posdata)
       (procedure? posdata-name)
       (procedure? posdata-line)
       (procedure? posdata-column)))

; charport
;------------------------------------------------------------------------------
(def-test "charport should initialize a charport properly"
  (call-with-input-string "a"
    (lambda (input)
      (define port (charport input))
      (and (equal? input (charport-port port))
           (equal? 1 (charport-line port))
           (equal? 1 (charport-column port))))))

; posdata=?
;------------------------------------------------------------------------------
(def-test "posdata=? should return true of the two objects are equal"
  (posdata=?
    (posdata "" 0 0)
    (posdata "" 0 0)))

(def-test "posdata=? should return false if name differs"
  (not
    (posdata=?
      (posdata "foo" 0 0)
      (posdata "bar" 0 0))))

(def-test "posdata=? should return false if line differs"
  (not
    (posdata=?
      (posdata "" 1 0)
      (posdata "" 2 0))))

(def-test "posdata=? should return false if column differs"
  (not
    (posdata=?
      (posdata "" 0 1)
      (posdata "" 0 2))))

; token=?
;------------------------------------------------------------------------------
(def-test "token=? should return true if tokens are equal"
  (token=?
    (token 'foo "" (posdata "" 0 0))
    (token 'foo "" (posdata "" 0 0))))

(def-test "token=? should return false if types differ"
  (not
    (token=?
      (token 'foo "" (posdata "" 0 0))
      (token 'bar "" (posdata "" 0 0)))))

(def-test "token=? should return false if text differs"
  (not
    (token=?
      (token 'foo "a" (posdata "" 0 0))
      (token 'foo "b" (posdata "" 0 0)))))

(def-test "token=? should return false if position data differs"
  (not
    (token=?
      (token 'foo "a" (posdata "" 0 0))
      (token 'foo "b" (posdata "" 1 0)))))

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

; charport-read
;------------------------------------------------------------------------------
(def-test "charport-read should increment column when character is not newline"
  (call-with-input-string ""
    (lambda (input)
      (define port (charport input))
      (and (eof-object? (charport-read port))
           (equal? 1 (charport-line port))
           (equal? 1 (charport-column port))))))

(def-test "charport-read should increment column when character is not newline"
  (call-with-input-string "a"
    (lambda (input)
      (define port (charport input))
      (define chobj (charport-read port))
      (and (charobj=? (charobj #\a (posdata "(string)" 1 2)) chobj)
           (equal? 1 (charport-line port))
           (equal? 2 (charport-column port))))))

(def-test "charport-read should increment line when character is newline"
  (call-with-input-string "\n"
    (lambda (input)
      (define port (charport input))
      (define chobj (charport-read port))
      (and (charobj=? (charobj #\newline (posdata "(string)" 2 1)) chobj)
           (equal? 2 (charport-line port))
           (equal? 1 (charport-column port))))))

; charport-pos
;------------------------------------------------------------------------------
(def-test "charport-pos should return psodata for given charport"
  (call-with-input-string "a"
    (lambda (input)
      (define prt (charport input))
      (define pos (charport-posdata prt))
      (and (equal? "(string)" (posdata-name pos))
           (equal? 1 (posdata-line pos))
           (equal? 1 (posdata-column pos))))))

; buf-posdata
;------------------------------------------------------------------------------
(def-test "buf-posdata should return posdata from charport"
  (call-with-input-string ""
    (lambda (input)
      (define prt (charport input))
      (define pos (buf-posdata prt))
      (and (equal? "(string)" (posdata-name pos))
           (equal? 1 (posdata-line pos))
           (equal? 1 (posdata-column pos))))))

(def-test "buf-posdata should return posdata from buf"
  (call-with-input-string ""
    (lambda (input)
      (define prt (buf (charport input) charport-read))
      (define pos (buf-posdata prt))
      (and (equal? "(string)" (posdata-name pos))
           (equal? 1 (posdata-line pos))
           (equal? 1 (posdata-column pos))))))

(def-test "buf-posdata should return posdata from multi layer buf"
  (call-with-input-string ""
    (lambda (input)
      (define prt (buf (buf (charport input) charport-read) dlang/tokenize))
      (define pos (buf-posdata prt))
      (and (equal? "(string)" (posdata-name pos))
           (equal? 1 (posdata-line pos))
           (equal? 1 (posdata-column pos))))))

(def-test "buf-posdata should error when an invalid object is encountered"
  (call-with-input-string ""
    (lambda (input)
      (check-exception "Argument was not a buf or a charport"
        (buf-posdata '())))))

; char-match
;------------------------------------------------------------------------------
(def-test "char-match should consume and return char if the next char matches"
  (call-with-input-string "a"
    (lambda (input)
      (define buffer (buf (charport input) charport-read))
      (and (equal? #\a (char-match buffer #\a))
           (eof-object? (buf-lookahead! buffer 1))))))

(def-test "char-match should error when EOF"
  (call-with-input-string ""
    (lambda (input)
      (define buffer (buf (charport input) charport-read))
      (check-exception "Expected 'a', received EOF instead"
        (char-match buffer #\a)))))

(def-test "char-match should error when chars do not match"
  (call-with-input-string "b"
    (lambda (input)
      (define buffer (buf (charport input) charport-read))
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
        (token 'id "a" (posdata "(string)" 1 2))))))

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
(def-test "token-matches? should return true if next token matches type"
  (call-with-input-string "abc"
    (lambda (input)
      (define buffer (dlang/lexer input))
      (token-matches? buffer 'id))))

(def-test "token-matches? should return false if next token does not match type"
  (call-with-input-string "ab"
    (lambda (input)
      (define buffer (dlang/lexer input))
      (not (token-matches? buffer 'foo)))))

(def-test "token-matches? should return false if EOF"
  (call-with-input-string ""
    (lambda (input)
      (define buffer (dlang/lexer input))
      (not (token-matches? buffer 'foo)))))

; keyword-match
;------------------------------------------------------------------------------
(def-test "keyword-match should consume and return if next token matches"
  (call-with-input-string "abc"
    (lambda (input)
      (define buffer (dlang/lexer input))
      (token=?
        (keyword-match buffer "abc")
        (token 'id "abc" (posdata "(string)" 1 2))))))

(def-test "keyword-match should error if next token not an id"
  (call-with-input-string "1.0"
    (lambda (input)
      (define buffer (dlang/lexer input))
      (check-exception "Expected 'abc', received '1.0' instead"
        (keyword-match buffer "abc")))))

(def-test "keyword-match should error if next token does not match"
  (call-with-input-string "ab"
    (lambda (input)
      (define buffer (dlang/lexer input))
      (check-exception "Expected 'abc', received 'ab' instead"
        (keyword-match buffer "abc")))))

(def-test "keyword-match should error if EOF"
  (call-with-input-string ""
    (lambda (input)
      (define buffer (dlang/lexer input))
      (check-exception "Expected 'abc', received EOF instead"
        (keyword-match buffer "abc")))))

; token->syntree
;------------------------------------------------------------------------------
(def-test "token->syntree should convert a token to a syntree"
  (syntree=?
    (token->syntree (token 'foo "bar" (posdata "" 0 0)))
    (syntree 'foo "bar" '())))

; test-apply
;------------------------------------------------------------------------------
(def-test "test-apply should return true if the input matches the applied rule"
  (call-with-input-string "abc"
    (lambda (input)
      (define buffer (dlang/lexer input))
      (test-apply dlang/expression buffer))))

(def-test "test-apply should return false if the applied rule fails"
  (call-with-input-string "abc"
    (lambda (input)
      (define buffer (dlang/lexer input))
      (not (test-apply dlang/arg-list buffer)))))

; collect-char
;------------------------------------------------------------------------------
(def-test "should return empty string if predicate function returns false"
  (call-with-input-string "abc"
    (lambda (input)
      (define buffer (buf (charport input) charport-read))
      (equal? "" (collect-char buffer dlang/integer?)))))

(def-test "should return empty string if predicate function returns false due to EOF"
  (call-with-input-string ""
    (lambda (input)
     (define buffer (buf (charport input) charport-read))
      (equal? "" (collect-char buffer dlang/integer?)))))

(def-test "should return string containing chars from buffer when predicate returns true"
  (call-with-input-string "123"
    (lambda (input)
     (define buffer (buf (charport input) charport-read))
      (equal? "123" (collect-char buffer dlang/integer?)))))

; consume-all
;------------------------------------------------------------------------------
(def-test "should consume nothing if predicate never returns true"
  (call-with-input-string "abc"
    (lambda (input)
      (define buffer (buf (charport input) charport-read))
      (consume-all buffer dlang/integer?)
      (charobj=? (charobj #\a (posdata "(string)" 1 2))
                 (buf-lookahead! buffer 1)))))

(def-test "should consume an item at a time until predicate returns false"
  (call-with-input-string "123a"
    (lambda (input)
      (define buffer (buf (charport input) charport-read))
      (consume-all buffer dlang/integer?)
      (charobj=? (charobj #\a (posdata "(string)" 1 5))
                 (buf-lookahead! buffer 1)))))

; collect
;------------------------------------------------------------------------------
(def-test "should return empty list if predicate never returns true"
  (call-with-input-string "abc"
    (lambda (input)
      (define buffer (buf (charport input) charport-read))
      (equal? '() (collect buffer dlang/integer? buf-consume!)))))

(def-test "should return list of items for which predicate returned true"
  (call-with-input-string "123"
    (lambda (input)
      (define buffer (buf (charport input) charport-read))
      (define result (collect buffer dlang/integer? buf-consume!))
      (equal? '(#\1 #\2 #\3)
              (map charobj-char result)))))

