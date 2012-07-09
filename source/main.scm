(declare (uses buf)
         (uses lexer)
         (uses parser))

(define program-parser
  (buf
    (buf (current-input-port) dlang/tokenize)
    dlang/program))

(define expression-parser
  (buf
    (buf (current-input-port) dlang/tokenize)
    dlang/program))

(print program-parser)
(print expression-parser)

