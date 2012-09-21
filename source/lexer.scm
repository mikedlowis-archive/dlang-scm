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

        ; Punctuation and Parens
        ((char=? ch #\()
         (token 'lpar (string (buf-consume! in)) location))
        ((char=? ch #\))
         (token 'rpar (string (buf-consume! in)) location))
        ((char=? ch #\,)
         (token 'comma (string (buf-consume! in)) location))
        ((char=? ch #\;)
         (token 'term (string (buf-consume! in)) location))

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
  (char-whitespace? (buf-lookahead! in 1)))

(define (dlang/comment in)
  (char-match in #\#)
  (consume-all in dlang/comment?)
  (dlang/tokenize in))

(define (dlang/comment? in)
  (and (not (char=? (buf-lookahead! in 1) #\newline))
       (not (eof-object? (buf-lookahead! in 1)))))

(define (dlang/number in)
  (define location (buf-posdata in))
  (token 'number
    (string-append
      (if (char=? #\- (buf-lookahead! in 1))
        (string (buf-consume! in)) "")
      (dlang/integer in)
      (if (char=? (buf-lookahead! in 1) #\.)
        (dlang/decimal in) "")
      (if (or (char=? (buf-lookahead! in 1) #\e)
              (char=? (buf-lookahead! in 1) #\E))
        (dlang/exponent in) ""))
    location))

(define (dlang/integer in)
  (if (dlang/integer? in)
      (collect-char in dlang/integer?)
      (abort "Expected an integer")))

(define (dlang/integer? in)
  (and (not (eof-object? (buf-lookahead! in 1)))
       (char-numeric? (charobj-char (buf-lookahead! in 1)))))

(define (dlang/decimal in)
  (string-append
    (string (char-match in #\.))
    (dlang/integer in)))

(define (dlang/exponent in)
  (string-append
    (string
      (if (char=? (buf-lookahead! in 1) #\e)
        (char-match in #\e) (char-match in #\E)))
    (if (char=? #\- (buf-lookahead! in 1))
      (string (buf-consume! in)) "")
    (dlang/integer in)))

(define (dlang/character in)
  (define location (buf-posdata in))
  (token 'character
    (string-append
      (string (char-match in #\'))
      (if (eof-object? (buf-lookahead! in 1))
        (abort "Unexpected EOF while parsing character literal")
        (string (buf-consume! in)))
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
       (not (char=? #\newline ch))
       (not (char=? #\" ch))))

(define (dlang/symbol in)
  (define location (buf-posdata in))
  (token 'symbol
    (string-append
      (string (char-match in #\$))
      (token-text (dlang/id in)))
    location))

(define (dlang/id in)
  (define location (buf-posdata in))
  (define str(collect-char in dlang/id-char?))
  (if (> (string-length str) 0)
    (token 'id str location)
    (abort "An Id was expected but none found.")))

(define (dlang/id-char? in)
  (define ch (buf-lookahead! in 1))
  (and (not (eof-object? ch))
       (not (char-whitespace? ch))
       (case ch
         ((#\( #\) #\; #\, #\' #\" #\$ #\#) #f)
         (else #t))))

