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

(define (dlang/number in str) 'number)
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
      (loop (string-append acc (buf-consume! in)) (buf-lookahead! in 1)))
    acc))


