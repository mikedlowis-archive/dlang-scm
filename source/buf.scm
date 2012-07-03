(declare (unit buf))

(define-record buf
  src
  fn
  pos
  marks
  data)

(define (buf src fn)
  (make-buf src fn 0 '() (vector)))

(define buf-struct? buf?)

(define (buf? obj)
  (and (buf-struct? obj)
       (procedure? (buf-fn obj))
       (integer?  (buf-pos obj))
       (list?     (buf-marks obj))
       (vector?   (buf-data obj))))

(define (buf-marked? b)
  (if (buf? b) '()))

(define (buf-mark b)
  (if (buf? b) '()))

(define (buf-release b)
  (if (buf? b) '()))

(define (buf-sync b n)
  (if (buf? b) '()))

(define (buf-fill b n)
  (if (buf? b) '()))

(define (buf-lookahead b n)
  (if (buf? b) '()))

(define (buf-consume b n)
  (if (buf? b) '()))

