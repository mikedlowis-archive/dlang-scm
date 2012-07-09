(include "test.scm")
(declare (unit test_lexer)
         (uses lexer))

; dlang/tokenize
;------------------------------------------------------------------------------

; dlang/whitespace
;------------------------------------------------------------------------------

; dlang/comment
;------------------------------------------------------------------------------

; dlang/number
;------------------------------------------------------------------------------

; dlang/integer
;------------------------------------------------------------------------------

; dlang/decimal
;------------------------------------------------------------------------------

; dlang/exponent
;------------------------------------------------------------------------------

; dlang/character
;------------------------------------------------------------------------------

; dlang/string
;------------------------------------------------------------------------------

; dlang/symbol
;------------------------------------------------------------------------------
(def-test "dlang/symbol should error when no name given for a symbol"
  (call-with-input-string "$"
    (lambda (input) '())))

(def-test "dlang/symbol should error when not a symbol"
  (call-with-input-string "abc"
    (lambda (input) '())))

(def-test "dlang/symbol should recognize a symbol of length one"
  (call-with-input-string "$a"
    (lambda (input) '())))

(def-test "dlang/symbol should recognize a symbol of length two"
  (call-with-input-string "$ab"
    (lambda (input) '())))

(def-test "dlang/symbol should recognize a symbol of length three"
  (call-with-input-string "$abc"
    (lambda (input) '())))

(def-test "dlang/symbol should stop recognition on EOF"
  (call-with-input-string "$abc"
    (lambda (input) '())))

(def-test "dlang/symbol should stop recognition on whitespace"
  (call-with-input-string "$abc "
    (lambda (input) '())))

; dlang/id
;------------------------------------------------------------------------------
(def-test "dlang/id should recognize an id of length one"
  (call-with-input-string "a"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/id buffer))
      (and (token? result)
           (equal? 'id (token-type result))
           (equal? "a" (token-text result))))))

(def-test "dlang/id should recognize an id of length two"
  (call-with-input-string "ab"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/id buffer))
      (and (token? result)
           (equal? 'id (token-type result))
           (equal? "ab" (token-text result))))))

(def-test "dlang/id should recognize an id of length three"
  (call-with-input-string "abc"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/id buffer))
      (and (token? result)
           (equal? 'id (token-type result))
           (equal? "abc" (token-text result))))))

(def-test "dlang/id should stop recognition on whitepsace"
  (call-with-input-string "abc abc"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/id buffer))
      (and (token? result)
           (equal? 'id (token-type result))
           (equal? "abc" (token-text result))))))

(def-test "dlang/id should stop recognition on EOF"
  (call-with-input-string "abc"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/id buffer))
      (and (token? result)
           (equal? 'id (token-type result))
           (equal? "abc" (token-text result))))))

(def-test "dlang/id should error when no id recognized"
  (call-with-input-string ""
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/id buffer))
      (equal? result "An Id was expected but none found.") )))

