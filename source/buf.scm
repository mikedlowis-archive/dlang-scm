(declare (unit buf))

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
  (if (buf? b)
    (> (length (buf-marks b)) 0)))

(define (buf-mark b)
  (if (buf? b)
    (buf-marks-set! b (cons (buf-pos b) (buf-marks b)))))

(define (buf-release b)
  (if (buf? b)
    (buf-marks-set! b (cdr (buf-marks b)))))

(define (buf-advance b)
  (if (buf? b)
    (buf-pos-set! b (+ 1 (buf-pos b)))))

(define (buf-sync b n)
  (if (buf? b)
    (let* ((pos     (buf-pos b))
           (size    (length (buf-data)))
           (nxt_idx (- (+ pos n) 1))
           (max_idx (- size 1)))
      (if (= size 0)
        (buf-fill b n)
        (if (>= nxt_idx max_idx)
          (buf-fill b (- nxt_idx max_idx)))))))

(define (buf-fill b n)
  (if (buf? b)
    (let loop ((i 0))
      ((buf-ldfn b))
      (if (< i n) (loop (+ i 1))))))

(define (buf-lookahead b n)
  (if (buf? b)
    (buf-sync b n)
    (vector-ref
      (buf-data b)
      (- (+ (buf-pos b) n) 1))))

(define (buf-consume b n)
  (if (buf? b)
    (begin
      (buf-advance b)
      (if
        (and
          (= location size)
          (not (buf-marked? b)))
        (begin
          (buf-pos-set! 0)
          (buf-data-set! (vector))))
      (buf-sync b1))))

