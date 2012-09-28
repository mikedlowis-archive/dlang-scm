(declare (unit lexer) (uses parse-utils))

(define (dlang/lexer input)
  (buf (dlang/char-buf input) dlang/tokenize))

(define (dlang/char-buf input)
  (buf (charport input) charport-read))

(define (dlang/tokenize in)
  (define location (buf-posdata in))
  (let ((ch (buf-lookahead! in 1)))
    (define tok
      (cond
        ; End of Input reached
        ((eof-object? ch) ch)

        ; Whitespace
        ((dlang/whitespace? in)
         (dlang/whitespace in))

        ; Comment
        ((chobj-char=? ch #\#)
         (dlang/comment in))

        ; Number
        ((or (and (chobj-char=? ch #\-) (dlang/integer? (buf-lookahead! in 2)))
             (char-numeric? (chobj-char ch)))
         (dlang/number in))

        ; Character
        ((chobj-char=? ch #\') (dlang/character in))

        ; String
        ((chobj-char=? ch #\") (dlang/string in))

        ; Symbol
        ((chobj-char=? ch #\$) (dlang/symbol in))

        ; Punctuation and Parens
        ((chobj-char=? ch #\()
         (token 'lpar (string (chobj-char (buf-consume! in))) location))
        ((chobj-char=? ch #\))
         (token 'rpar (string (chobj-char (buf-consume! in))) location))
        ((chobj-char=? ch #\,)
         (token 'comma (string (chobj-char (buf-consume! in))) location))
        ((chobj-char=? ch #\;)
         (token 'term (string (chobj-char (buf-consume! in))) location))

        ; Id
        (else
          (dlang/id in))))
    (if (and (not (eof-object? tok))
             (equal? "end" (token-text tok)))
        (token-type-set! tok 'term))
    tok))

(define (dlang/whitespace in)
  (consume-all in dlang/whitespace?)
  (dlang/tokenize in))

(define (dlang/whitespace? in)
  (and (not (eof-object? (buf-lookahead! in 1)))
       (char-whitespace? (chobj-char (buf-lookahead! in 1)))))

(define (dlang/comment in)
  (char-match in #\#)
  (consume-all in dlang/comment?)
  (dlang/tokenize in))

(define (dlang/comment? in)
  (and (not (eof-object? (buf-lookahead! in 1)))
       (not (chobj-char=? (buf-lookahead! in 1) #\newline))))

(define (dlang/number in)
  (define location (buf-posdata in))
  (token 'number
    (string-append
      (if (chobj-char=? (buf-lookahead! in 1) #\-)
          (string (chobj-char (buf-consume! in))) "")
      (dlang/integer in)
      (if (chobj-char=? (buf-lookahead! in 1) #\.)
          (dlang/decimal in) "")
      (if (or (chobj-char=? (buf-lookahead! in 1) #\e)
              (chobj-char=? (buf-lookahead! in 1) #\E))
          (dlang/exponent in) ""))
    location))

(define (dlang/integer in)
  (if (dlang/integer? in)
      (collect-char in dlang/integer?)
      (abort "Expected an integer")))

(define (dlang/integer? in)
  (and (not (eof-object? (buf-lookahead! in 1)))
       (char-numeric? (chobj-char (buf-lookahead! in 1)))))

(define (dlang/decimal in)
  (string-append
    (string (char-match in #\.))
    (dlang/integer in)))

(define (dlang/exponent in)
  (string-append
    (string
      (if (and (not (eof-object? (buf-lookahead! in 1)))
               (char=? #\e (chobj-char (buf-lookahead! in 1))))
        (char-match in #\e)
        (char-match in #\E)))
    (if (and (not (eof-object? (buf-lookahead! in 1)))
             (char=? #\- (chobj-char (buf-lookahead! in 1))))
      (string (chobj-char (buf-consume! in))) "")
    (dlang/integer in)))

(define (dlang/character in)
  (define location (buf-posdata in))
  (token 'character
    (string-append
      (string (char-match in #\'))
      (if (eof-object? (buf-lookahead! in 1))
        (abort "Unexpected EOF while parsing character literal")
        (string (chobj-char (buf-consume! in))))
      (string (char-match in #\')))
    location))

(define (dlang/string in)
  (define location (buf-posdata in))
  (define text
    (string-append
      (string (char-match in #\"))
      (collect-char in dlang/string-char?)
      (string (char-match in #\"))))
  (token 'string text location))

(define (dlang/string-char? in)
  (define ch (buf-lookahead! in 1))
  (and (not (eof-object? ch))
       (not (char=? #\newline (chobj-char ch)))
       (not (char=? #\" (chobj-char ch)))))

(define (dlang/symbol in)
  (define location (buf-posdata in))
  (token 'symbol
    (string-append
      (string (char-match in #\$))
      (token-text (dlang/id in)))
    location))

(define (dlang/id in)
  (define location (buf-posdata in))
  (define str (collect-char in dlang/id-char?))
  (if (> (string-length str) 0)
    (token 'id str location)
    (abort "An Id was expected but none found.")))

(define (dlang/id-char? in)
  (define ch (buf-lookahead! in 1))
  (and (not (eof-object? ch))
       (not (char-whitespace? (chobj-char ch)))
       (case (chobj-char ch)
         ((#\( #\) #\; #\, #\' #\" #\$ #\#) #f)
         (else #t))))

