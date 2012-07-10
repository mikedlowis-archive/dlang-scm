(include "test.scm")
(declare (unit test_lexer)
         (uses lexer))

; dlang/tokenize
;------------------------------------------------------------------------------

; dlang/whitespace
;------------------------------------------------------------------------------
(def-test "dlang/whitespace should recognize and consume whitespace"
  (call-with-input-string " \t\r\na"
    (lambda (input) '())))

; dlang/comment
;------------------------------------------------------------------------------
(def-test "dlang/comment should recognize comments with windows style line endings"
  (call-with-input-string "# foo\r\n"
    (lambda (input) '())))

(def-test "dlang/comment should recognize comments with unix style line endings"
  (call-with-input-string "# foo\n"
    (lambda (input) '())))

(def-test "dlang/comment should recognize an empty comment"
  (call-with-input-string "#\n"
    (lambda (input) '())))

(def-test "dlang/comment should recognize comment at EOF"
  (call-with-input-string "#"
    (lambda (input) '())))

; dlang/number
;------------------------------------------------------------------------------
(def-test "dlang/number should recognize a positive integer"
  (call-with-input-string "1"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/number buffer))
      (and (token? result)
           (equal? 'number (token-type result))
           (equal? "1" (token-text result))))))

(def-test "dlang/number should recognize a negative integer"
  (call-with-input-string "-1"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/number buffer))
      (and (token? result)
           (equal? 'number (token-type result))
           (equal? "-1" (token-text result))))))

(def-test "dlang/number should recognize a positive float"
  (call-with-input-string "1.1"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/number buffer))
      (and (token? result)
           (equal? 'number (token-type result))
           (equal? "1.1" (token-text result))))))

(def-test "dlang/number should recognize a negative float"
  (call-with-input-string "-1.1"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/number buffer))
      (and (token? result)
           (equal? 'number (token-type result))
           (equal? "-1.1" (token-text result))))))

(def-test "dlang/number should recognize a positive integer with positive exponent"
  (call-with-input-string "1e1"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/number buffer))
      (and (token? result)
           (equal? 'number (token-type result))
           (equal? "1e1" (token-text result))))))

(def-test "dlang/number should recognize a positive integer with negative exponent"
  (call-with-input-string "1e-1"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/number buffer))
      (and (token? result)
           (equal? 'number (token-type result))
           (equal? "1e-1" (token-text result))))))

(def-test "dlang/number should recognize a positive float with positive exponent"
  (call-with-input-string "1.1e1"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/number buffer))
      (and (token? result)
           (equal? 'number (token-type result))
           (equal? "1.1e1" (token-text result))))))

(def-test "dlang/number should recognize a positive float with negative exponent"
  (call-with-input-string "1.1e-1"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/number buffer))
      (and (token? result)
           (equal? 'number (token-type result))
           (equal? "1.1e-1" (token-text result))))))

(def-test "dlang/number should recognize a negative integer with positive exponent"
  (call-with-input-string "-1e1"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/number buffer))
      (and (token? result)
           (equal? 'number (token-type result))
           (equal? "-1e1" (token-text result))))))

(def-test "dlang/number should recognize a negative integer with negative exponent"
  (call-with-input-string "-1e-1"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/number buffer))
      (and (token? result)
           (equal? 'number (token-type result))
           (equal? "-1e-1" (token-text result))))))

(def-test "dlang/number should recognize a negative float with positive exponent"
  (call-with-input-string "-1.1e1"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/number buffer))
      (and (token? result)
           (equal? 'number (token-type result))
           (equal? "-1.1e1" (token-text result))))))

(def-test "dlang/number should recognize a negative float with negative exponent"
  (call-with-input-string "-1.1e-1"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/number buffer))
      (and (token? result)
           (equal? 'number (token-type result))
           (equal? "-1.1e-1" (token-text result))))))

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
  (call-with-input-string ""
    (lambda (input)
      (define buffer (buf input read-char))
      (check-error "Expected an integer"
        (dlang/integer buffer)))))

; dlang/decimal
;------------------------------------------------------------------------------
(def-test "dlang/decimal should recognize an decimal of length one"
  (call-with-input-string ".0"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/decimal buffer))
      (and (string? result)
           (equal? ".0" result)))))

(def-test "dlang/decimal should recognize an decimal of length two"
  (call-with-input-string ".01"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/decimal buffer))
      (and (string? result)
           (equal? ".01" result)))))

(def-test "dlang/decimal should recognize an decimal of length three"
  (call-with-input-string ".012"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/decimal buffer))
      (and (string? result)
           (equal? ".012" result)))))

(def-test "dlang/decimal should error when no integer portion in input"
  (call-with-input-string ". "
    (lambda (input)
      (define buffer (buf input read-char))
      (check-error "Expected an integer"
        (dlang/decimal buffer)))))

(def-test "dlang/decimal should error when EOF"
  (call-with-input-string ""
    (lambda (input)
      (define buffer (buf input read-char))
      (check-error "Expected '.', received EOF instead"
        (dlang/decimal buffer)))))

; dlang/exponent
;------------------------------------------------------------------------------
(def-test "dlang/exponent should recognize an exponent of length one"
  (call-with-input-string "e0"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/exponent buffer))
      (and (string? result)
           (equal? "e0" result)))))

(def-test "dlang/exponent should recognize an exponent of length two"
  (call-with-input-string "e01"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/exponent buffer))
      (and (string? result)
           (equal? "e01" result)))))

(def-test "dlang/exponent should recognize an exponent of length three"
  (call-with-input-string "e012"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/exponent buffer))
      (and (string? result)
           (equal? "e012" result)))))

(def-test "dlang/exponent should recognize an exponent with uppercase E"
  (call-with-input-string "E012"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/exponent buffer))
      (and (string? result)
           (equal? "E012" result)))))

(def-test "dlang/exponent should recognize a negative exponent"
  (call-with-input-string "e-012"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/exponent buffer))
      (and (string? result)
           (equal? "e-012" result)))))

(def-test "dlang/exponent should error when no integer portion in input"
  (call-with-input-string "e "
    (lambda (input)
      (define buffer (buf input read-char))
      (check-error "Expected an integer"
        (dlang/exponent buffer)))))

(def-test "dlang/exponent should error when EOF"
  (call-with-input-string ""
    (lambda (input)
      (define buffer (buf input read-char))
      (check-error "Expected 'E', received EOF instead"
        (dlang/exponent buffer)))))

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

(def-test "dlang/stop recognition when comment encountered"
  (call-with-input-string "foo#"
    (lambda (input) '())))



