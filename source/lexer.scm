(declare (unit lexer)
         (uses buf))
(include "loop.scm")

(define (dlang/tokenize in)
  (let ((ch (buf-lookahead! in 1)))
    (cond
      ; Whitespace
      ((char-whitespace? ch)
       (dlang/whitespace in)
       (dlang/tokenize in))

      ; Comment
      ((char=? ch #\#)
       (dlang/comment in)
       (dlang/tokenize in))

      ; Number
      ((or
         (and (char=? ch #\-) (char-numeric? (buf-lookahead! in 2)))
         (char-numeric? ch))
       (dlang/number in))

      ; Character
      ((char=? ch #\') (dlang/character in ""))

      ; String
      ((char=? ch #\") (dlang/string in ""))

      ; Symbol
      ((char=? ch #\$) (dlang/symbol in ""))

      ; Parentheses
      ((char=? ch #\()
       (token 'lpar (string (buf-consume! in))))
      ((char=? ch #\))
       (token 'rpar (string (buf-consume! in))))

      ; Id
      (else
        (dlang/id in)))))

(define (dlang/whitespace in)
  (while (char-whitespace? (buf-lookahead! in 1))
    (buf-consume! in)))

(define (dlang/comment in)
  (match in #\#)
  (while (not (char=? (buf-lookahead! in) #\newline))
    (buf-consume! in)))

(define (dlang/number in)
  (lex/token 'number
    (string-append
      (if (char=? #\- (buf-lookahead! in 1)) (buf-consume! in) "")
      (dlang/integer in)
      (if (char=? (buf-lookahead! in 1) #\.)
        (dlang/decimal in) "")
      (if (or (char=? (buf-lookahead! in 1) #\e)
              (char=? (buf-lookahead! in 1) #\E))
        (dlang/exponent in) ""))))

(define (dlang/integer in)
  (define text "")
  (if (char-numeric? (buf-lookahead! in 1))
    (while (char-numeric? (buf-lookahead! in 1))
      (set! text (string-append text (string (buf-consume! in)))))
    (error "Expected a number."))
  text)

(define (dlang/decimal in)
  (string-append
    (match in #\.)
    (dlang/digits in "")))

(define (dlang/exponent in)
  (string-append
    (if (char=? (buf-lookahead! in 1) #\e)
      (match in #\e) (match in #\E))
    (dlang/integer in "")))

(define (dlang/character in str)
  (token 'character
    (string-append
      (string (match in #\'))
      (string (buf-consume! in))
      (string (match in #\')) )))

(define (dlang/string in str)
  (token 'string
    (string-append
      (string (match in #\"))
      (accumulate-till in string-append "" #\")
      (string (match in #\")) )))

(define (dlang/symbol in str)
  (token 'symbol
    (string-append
      (match in #\$)
      (token-text (dlang/id in)))))

(define (dlang/id in)
  (let loop ((acc "")
             (ch  (buf-lookahead! in 1)))
    (if
      (and (not (char-whitespace? ch))
           (not (eof-object? ch)))
      (loop (string-append acc (buf-consume! in)) (buf-lookahead! in 1))
      (token 'id acc))))

