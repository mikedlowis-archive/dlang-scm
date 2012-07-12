(include "test.scm")
(declare (unit test_lexer)
         (uses lexer))

; dlang/lexer
;------------------------------------------------------------------------------
(def-test "dlang/lexer should create a new lexer from the given port"
  (call-with-input-string "abc"
    (lambda (input)
      (define buffer (dlang/lexer input))
      (and (buf? buffer)
           (buf? (buf-src buffer))
           (token? (buf-lookahead! buffer 1))))))

(def-test "dlang/lexer should create a capable of returning a sequence of tokens"
  (call-with-input-string "abc 123 $foo"
    (lambda (input)
      (define buffer (dlang/lexer input))
      (and (buf? buffer)
           (buf? (buf-src buffer))
           (token? (buf-lookahead! buffer 1))
           (token? (buf-lookahead! buffer 2))
           (token? (buf-lookahead! buffer 3))))))

(def-test "dlang/lexer should create a lexer that returns the #!eof when EOF reached"
  (call-with-input-string "abc"
    (lambda (input)
      (define buffer (dlang/lexer input))
      (and (buf? buffer)
           (buf? (buf-src buffer))
           (token? (buf-lookahead! buffer 1))
           (eof-object? (buf-lookahead! buffer 2))
           (eof-object? (buf-lookahead! buffer 3))))))

; dlang/tokenize
;------------------------------------------------------------------------------
(def-test "dlang/tokenize should recognize whitespace"
  (call-with-input-string " \t\r\n"
    (lambda (input)
      (define buffer (buf input read-char))
      (eof-object? (dlang/tokenize buffer)))))

(def-test "dlang/tokenize should recognize a comment"
  (call-with-input-string "# foo"
    (lambda (input)
      (define buffer (buf input read-char))
      (eof-object? (dlang/tokenize buffer)))))

(def-test "dlang/tokenize should recognize a number"
  (call-with-input-string "12"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/tokenize buffer))
      (and (token? result)
           (equal? 'number (token-type result))
           (equal? "12" (token-text result))))))

(def-test "dlang/tokenize should recognize a character"
  (call-with-input-string "'a'"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/tokenize buffer))
      (and (token? result)
           (equal? 'character (token-type result))
           (equal? "'a'" (token-text result))))))

(def-test "dlang/tokenize should recognize a string"
  (call-with-input-string "\"\""
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/tokenize buffer))
      (and (token? result)
           (equal? 'string (token-type result))
           (equal? "\"\"" (token-text result))))))

(def-test "dlang/tokenize should recognize a symbol"
  (call-with-input-string "$foobar"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/tokenize buffer))
      (and (token? result)
           (equal? 'symbol (token-type result))
           (equal? "$foobar" (token-text result))))))

(def-test "dlang/tokenize should recognize an id"
  (call-with-input-string "foobar"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/tokenize buffer))
      (and (token? result)
           (equal? 'id (token-type result))
           (equal? "foobar" (token-text result))))))

(def-test "dlang/tokenize should recognize the EOF"
  (call-with-input-string ""
    (lambda (input)
      (define buffer (buf input read-char))
      (eof-object? (dlang/tokenize buffer)))))

(def-test "dlang/tokenize should recognize a left parenthese"
  (call-with-input-string "("
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/tokenize buffer))
      (and (token? result)
           (equal? 'lpar (token-type result))
           (equal? "(" (token-text result))))))

(def-test "dlang/tokenize should recognize a right parenthese"
  (call-with-input-string ")"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/tokenize buffer))
      (and (token? result)
           (equal? 'rpar (token-type result))
           (equal? ")" (token-text result))))))

(def-test "dlang/tokenize should recognize a comma"
  (call-with-input-string ","
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/tokenize buffer))
      (and (token? result)
           (equal? 'comma (token-type result))
           (equal? "," (token-text result))))))

(def-test "dlang/tokenize should recognize a semicolon"
  (call-with-input-string ";"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/tokenize buffer))
      (and (token? result)
           (equal? 'term (token-type result))
           (equal? ";" (token-text result))))))

(def-test "dlang/tokenize should recognize the end keyword"
  (call-with-input-string "end"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/tokenize buffer))
      (and (token? result)
           (equal? 'term (token-type result))
           (equal? "end" (token-text result))))))

; dlang/whitespace
;------------------------------------------------------------------------------
(def-test "dlang/whitespace should recognize and consume whitespace"
  (call-with-input-string " \t\r\n"
    (lambda (input)
      (define buffer (buf input read-char))
      (eof-object? (dlang/whitespace buffer)))))

(def-test "dlang/whitespace continue parsing after whitespace"
  (call-with-input-string " \t\r\nfoo"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/whitespace buffer))
      (and (token? result)
           (equal? 'id (token-type result))
           (equal? "foo" (token-text result))))))

; dlang/comment
;------------------------------------------------------------------------------
(def-test "dlang/comment should recognize comments with windows style line endings"
  (call-with-input-string "# foo\r\n"
    (lambda (input)
      (define buffer (buf input read-char))
      (eof-object? (dlang/comment buffer)))))

(def-test "dlang/comment should recognize comments with unix style line endings"
  (call-with-input-string "# foo\n"
    (lambda (input)
      (define buffer (buf input read-char))
      (eof-object? (dlang/comment buffer)))))

(def-test "dlang/comment should recognize an empty comment"
  (call-with-input-string "#\n"
    (lambda (input)
      (define buffer (buf input read-char))
      (eof-object? (dlang/comment buffer)))))

(def-test "dlang/comment should recognize comment at EOF"
  (call-with-input-string "#"
    (lambda (input)
      (define buffer (buf input read-char))
      (eof-object? (dlang/comment buffer)))))

(def-test "dlang/comment should continue parsing after a comment"
  (call-with-input-string "# foo\r\nbar"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/comment buffer))
      (and (token? result)
           (equal? 'id (token-type result))
           (equal? "bar" (token-text result))))))

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
(def-test "dlang/character should recognize a character"
  (call-with-input-string "'a'"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/character buffer))
      (and (token? result)
           (equal? 'character (token-type result))
           (equal? "'a'" (token-text result))))))

(def-test "dlang/character should error when missing first single quote"
  (call-with-input-string "a'"
    (lambda (input)
      (define buffer (buf input read-char))
      (check-error "Expected ''', received 'a' instead"
        (dlang/character buffer)))))

(def-test "dlang/character should error when missing second single quote"
  (call-with-input-string "'a"
    (lambda (input)
      (define buffer (buf input read-char))
      (check-error "Expected ''', received EOF instead"
        (dlang/character buffer)))))

(def-test "dlang/character should error when EOF reached"
  (call-with-input-string "'"
    (lambda (input)
      (define buffer (buf input read-char))
      (check-error "Unexpected EOF while parsing character literal"
        (dlang/character buffer)))))

; dlang/string
;------------------------------------------------------------------------------
(def-test "dlang/string should recognize an empty string"
  (call-with-input-string "\"\""
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/string buffer))
      (and (token? result)
           (equal? 'string (token-type result))
           (equal? "\"\"" (token-text result))))))

(def-test "dlang/string should recognize a string of length 1"
  (call-with-input-string "\"a\""
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/string buffer))
      (and (token? result)
           (equal? 'string (token-type result))
           (equal? "\"a\"" (token-text result))))))

(def-test "dlang/string should recognize a string of length 2"
  (call-with-input-string "\"ab\""
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/string buffer))
      (and (token? result)
           (equal? 'string (token-type result))
           (equal? "\"ab\"" (token-text result))))))

(def-test "dlang/string should recognize a string of length 3"
  (call-with-input-string "\"abc\""
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/string buffer))
      (and (token? result)
           (equal? 'string (token-type result))
           (equal? "\"abc\"" (token-text result))))))

(def-test "dlang/string should error when missing first double quote"
  (call-with-input-string "a\""
    (lambda (input)
      (define buffer (buf input read-char))
      (check-error "Expected '\"', received 'a' instead"
        (dlang/string buffer)))))

(def-test "dlang/string should error when missing second double quote"
  (call-with-input-string "\"a"
    (lambda (input)
      (define buffer (buf input read-char))
      (check-error "Expected '\"', received EOF instead"
        (dlang/string buffer)))))

(def-test "dlang/string should error when EOF reached"
  (call-with-input-string "\""
    (lambda (input)
      (define buffer (buf input read-char))
      (check-error "Expected '\"', received EOF instead"
        (dlang/string buffer)))))

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

(def-test "dlang/id should stop recognition when comment encountered"
  (call-with-input-string "foo#"
    (lambda (input)
      (define buffer (buf input read-char))
      (define result (dlang/id buffer))
      (and (token? result)
           (equal? 'id (token-type result))
           (equal? "foo" (token-text result))))))

