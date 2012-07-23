(declare (uses lexer parser scheme))

(define (parse-file fname)
  (define result (dlang/program (dlang/lexer (open-input-file fname))))
  (set! result (scheme-program result))
  result)

; If we have a file, then parse it
(if (= 1 (length (command-line-arguments)))
  (map print (parse-file (car (command-line-arguments))))
  (print "No input file provided."))

