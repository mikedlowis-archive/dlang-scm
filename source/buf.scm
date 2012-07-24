(declare (unit buf) (uses library))

(define-record buf
  src ldfn pos marks data)

(define (buf src fn)
  (make-buf src fn 0 '() (vector)))

(define buf-struct? buf?)

(define (buf? obj)
  (and (buf-struct? obj)
       (procedure? (buf-ldfn obj))
       (integer?  (buf-pos obj))
       (list?     (buf-marks obj))
       (vector?   (buf-data obj))))

(define (vector-append v1 v2 . vN)
  (define new-vec (list->vector (append (vector->list v1) (vector->list v2))))
  (if (null? vN) new-vec (vector-append new-vec (car vN) (cdr vN))))

(define (buf-marked? b)
  (> (length (buf-marks b)) 0))

(define (buf-mark! b)
  (buf-marks-set! b (cons (buf-pos b) (buf-marks b))))

(define (buf-release! b)
  (buf-pos-set! b (car (buf-marks b)))
  (buf-marks-set! b (cdr (buf-marks b))))

(define (buf-advance! b)
  (buf-pos-set! b (+ 1 (buf-pos b))))

(define (buf-fill! b n)
  (let loop ((i 0))
    (if (< i n)
      (begin
        (buf-data-set! b
          (vector-append
            (buf-data b)
            (vector ((buf-ldfn b) (buf-src b)))))
        (loop (+ i 1))))))

(define (buf-sync! b n)
  (let* ((pos     (buf-pos b))
         (size    (vector-length (buf-data b)))
         (nxt_idx (- (+ pos n) 1))
         (max_idx (- size 1)))
    (if (= size 0)
      (buf-fill! b n)
      (if (>= nxt_idx max_idx)
        (buf-fill! b (- nxt_idx max_idx))))))

(define (buf-lookahead! b n)
  (buf-sync! b n)
  (vector-ref
    (buf-data b)
    (- (+ (buf-pos b) n) 1)))

(define (buf-consume! b)
  (define current (buf-lookahead! b 1))
  (buf-advance! b)
  (if
    (and
      (= (buf-pos b) (vector-length (buf-data b)))
      (not (buf-marked? b)))
    (begin
      (buf-pos-set! b 0)
      (buf-data-set! b (vector))))
  (buf-sync! b 1)
  current)

