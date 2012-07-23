(declare (uses lexer parser scheme))

; Require the String library extension
(require-extension srfi-13)

(define (get-output-file-name ifname)
  (string-append (substring ifname 0 (string-index-right ifname #\.)) ".scm"))

(define (parse-file fname)
  (scheme-program (dlang/program (dlang/lexer (open-input-file fname)))))

(define (interpret-file fname)
  (define ofname (get-output-file-name fname))
  (define program (parse-file fname))
  (with-output-to-file ofname
    (lambda () (map print program)))
  (load ofname))

; If we have a file, then parse it
(if (= 1 (length (command-line-arguments)))
  (interpret-file (car (command-line-arguments)))
  ;(map print (parse-file (car (command-line-arguments))))
  (print "No input file provided."))

