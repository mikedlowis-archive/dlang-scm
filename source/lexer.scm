(include "loop.scm")
(declare (unit lexer)
         (uses parse-utils)
         (uses buf))

(define (dlang/tokenize in)
  (let ((ch (buf-lookahead! in 1)))
    (cond
      ; End of Input reached
      ((eof-object? ch) ch)

      ; Whitespace
      ((char-whitespace? ch)
       (dlang/whitespace in))

      ; Comment
      ((char=? ch #\#)
       (dlang/comment in))

      ; Number
      ((or
         (and (char=? ch #\-) (char-numeric? (buf-lookahead! in 2)))
         (char-numeric? ch))
       (dlang/number in))

      ; Character
      ((char=? ch #\') (dlang/character in))

      ; String
      ((char=? ch #\") (dlang/string in))

      ; Symbol
      ((char=? ch #\$) (dlang/symbol in))

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
    (buf-consume! in))
  (dlang/tokenize in))

(define (dlang/comment in)
  (char-match in #\#)
  (while (and (not (char=? (buf-lookahead! in 1) #\newline))
              (not (eof-object? (buf-lookahead! in 1))))
    (buf-consume! in))
  (dlang/tokenize in))

(define (dlang/number in)
  (token 'number
    (string-append
      (if (char=? #\- (buf-lookahead! in 1))
        (string (buf-consume! in)) "")
      (dlang/integer in)
      (if (char=? (buf-lookahead! in 1) #\.)
        (dlang/decimal in) "")
      (if (or (char=? (buf-lookahead! in 1) #\e)
              (char=? (buf-lookahead! in 1) #\E))
        (dlang/exponent in) ""))))

(define (dlang/integer in)
  (define text "")
  (if (and
        (not (eof-object? (buf-lookahead! in 1)))
        (char-numeric? (buf-lookahead! in 1)))
    (while (char-numeric? (buf-lookahead! in 1))
      (set! text (string-append text (string (buf-consume! in)))))
    (error "Expected an integer"))
  text)

(define (dlang/decimal in)
  (string-append
    (string (char-match in #\.))
    (dlang/integer in)))

(define (dlang/exponent in)
  (string-append
    ;(string (char-match-one-of in "eE"))
    (string
      (if (char=? (buf-lookahead! in 1) #\e)
        (char-match in #\e) (char-match in #\E)))
    (if (char=? #\- (buf-lookahead! in 1))
      (string (buf-consume! in)) "")
    (dlang/integer in)))

(define (dlang/character in)
  (token 'character
    (string-append
      (string (char-match in #\'))
      (if (eof-object? (buf-lookahead! in 1))
        (error "Unexpected EOF while parsing character literal")
        (string (buf-consume! in)))
      (string (char-match in #\')) )))

(define (dlang/string in)
  (define text (string (char-match in #\")))
  (while (and (not (eof-object? (buf-lookahead! in 1)))
              (not (char=? #\newline (buf-lookahead! in 1)))
              (not (char=? #\" (buf-lookahead! in 1))))
    (set! text (string-append text (string (buf-consume! in)))))
  (set! text (string-append text (string (char-match in #\"))))
  (token 'string text))

(define (dlang/symbol in)
  (token 'symbol
    (string-append
      (string (char-match in #\$))
      (token-text (dlang/id in)))))

(define (dlang/id in)
  (define acc "")
  (define ch (buf-lookahead! in 1))
  (while (and (not (char-whitespace? ch))
           (not (eof-object? ch))
           (not (char=? ch #\#)))
    (set! acc (string-append acc (string (buf-consume! in))))
    (set! ch (buf-lookahead! in 1)))
  (if (> (string-length acc) 0)
    (token 'id acc)
    (error "An Id was expected but none found.")))

