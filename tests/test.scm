(declare (unit test)
         (compile-syntax))

(define unit-tests '())

(define (register-test! test)
  (set! unit-tests (append unit-tests (list test))))

(define (error msg) msg)

(define (print-summary pass fail)
  (if (zero? fail)
    (print "Success: " pass " tests passed.")
    (print "Failure: " fail " / " (+ pass fail) " tests failed.")))

(define (run-all-unit-tests)
  (let loop ((pass  0)
             (fail  0)
             (tests unit-tests))
    (if (not (null? tests))
      (let* ((test (car tests))
             (desc (car test))
             (fn   (cdr test)))
        (if (not (equal? #t (fn)))
          (begin
            (print "FAIL: " desc)
            (loop pass (+ fail 1) (cdr tests)))
          (loop (+ pass 1) fail (cdr tests))))
      (print-summary pass fail))))

