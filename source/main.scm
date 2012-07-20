(declare (uses parser))

(define (parse-file fname)
  (define result
    (dlang/program (dlang/lexer (open-input-file fname))))
  (print result))

; If we have a file, then parse it
(if (= 1 (length (command-line-arguments)))
  (parse-file (car (command-line-arguments)))
  (print "No input file provided."))

