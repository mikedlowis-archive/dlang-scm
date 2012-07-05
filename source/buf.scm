(declare (unit buf)
         (uses library))

(use vector-lib)

(define-record buf
  src
  ldfn
  pos
  marks
  data)

(define (buf src fn)
  (make-buf src fn 0 '() (vector)))

(define buf-struct? buf?)

(define (buf? obj)
  (and (buf-struct? obj)
       (procedure? (buf-ldfn obj))
       (integer?  (buf-pos obj))
       (list?     (buf-marks obj))
       (vector?   (buf-data obj))))

(define (buf-marked? b)
  (> (length (buf-marks b)) 0))

(define (buf-mark! b)
  (buf-marks-set! b (cons (buf-pos b) (buf-marks b))))

(define (buf-release! b)
  (buf-marks-set! b (cdr (buf-marks b))))

(define (buf-advance! b)
  (buf-pos-set! b (+ 1 (buf-pos b))))

(define (buf-fill! b n)
  (let loop ((i 0))
    (buf-data-set!
      (vector-append (buf-data b) ((buf-ldfn b) (buf-src b))))
    (if (< i n) (loop (+ i 1)))))

(define (buf-sync! b n)
  (let* ((pos     (buf-pos b))
         (size    (length (buf-data)))
         (nxt_idx (- (+ pos n) 1))
         (max_idx (- size 1)))
    (if (= size 0)
      (buf-fill! b n)
      (if (>= nxt_idx max_idx)
        (buf-fill! b (- nxt_idx max_idx))))))

(define (buf-lookahead! b n)
  (buf-sync b n)
  (vector-ref
    (buf-data b)
    (- (+ (buf-pos b) n) 1)))

(define (buf-consume! b n)
  (buf-advance! b)
  (if
    (and
      (= (buf-pos b) (length (buf-data b)))
      (not (buf-marked? b)))
    (begin
      (buf-pos-set! 0)
      (buf-data-set! (vector))))
  (buf-sync! b1))

