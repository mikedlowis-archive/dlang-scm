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
(def-test "dlang/integer should recognize an integer of length one"
  (call-with-input-string "0"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/integer buffer))
      (and (string? result)
           (equal? "0" result)))))

(def-test "dlang/integer should recognize an integer of length two"
  (call-with-input-string "01"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/integer buffer))
      (and (string? result)
           (equal? "01" result)))))

(def-test "dlang/integer should recognize an integer of length three"
  (call-with-input-string "012"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/integer buffer))
      (and (string? result)
           (equal? "012" result)))))

(def-test "dlang/integer should error when no integer in input"
  (call-with-input-string "abc"
    (lambda (input)
      (define buffer (buf input read-char))
      (check-error "Expected an integer"
        (dlang/integer buffer)))))

(def-test "dlang/integer should error when EOF"
  (call-with-input-string "abc"
    (lambda (input)
      (define buffer (buf input read-char))
      (check-error "Expected an integer"
        (dlang/integer buffer)))))

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
(def-test "dlang/symbol should recognize a symbol of length one"
  (call-with-input-string "$a"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/symbol buffer))
      (and (token? result)
           (equal? 'symbol (token-type result))
           (equal? "$a" (token-text result))))))

(def-test "dlang/symbol should recognize a symbol of length two"
  (call-with-input-string "$ab"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/symbol buffer))
      (and (token? result)
           (equal? 'symbol (token-type result))
           (equal? "$ab" (token-text result))))))

(def-test "dlang/symbol should recognize a symbol of length three"
  (call-with-input-string "$abc"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/symbol buffer))
      (and (token? result)
           (equal? 'symbol (token-type result))
           (equal? "$abc" (token-text result))))))

(def-test "dlang/symbol should stop recognition on EOF"
  (call-with-input-string "$abc"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/symbol buffer))
      (and (token? result)
           (equal? 'symbol (token-type result))
           (equal? "$abc" (token-text result))))))

(def-test "dlang/symbol should stop recognition on whitespace"
  (call-with-input-string "$abc "
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/symbol buffer))
      (and (token? result)
           (equal? 'symbol (token-type result))
           (equal? "$abc" (token-text result))))))

(def-test "dlang/symbol should error when no name given for a symbol"
  (call-with-input-string "$"
    (lambda (input)
      (define buffer (buf input read-char))
      (check-error "An Id was expected but none found."
        (dlang/symbol buffer)))))

(def-test "dlang/symbol should error when EOF"
  (call-with-input-string ""
    (lambda (input)
      (define buffer (buf input read-char))
      (check-error "Expected '$', received EOF instead"
        (dlang/symbol buffer)))))

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
      (check-error "An Id was expected but none found."
        (dlang/id buffer)))))

