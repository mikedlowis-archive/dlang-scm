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

; token-match
;------------------------------------------------------------------------------

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

