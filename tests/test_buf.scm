(include "test.scm")
(declare (unit test_buf)
         (uses buf)
         (uses ports)
         (uses test))

; buf
;------------------------------------------------------------------------------
(def-test "buf should create a new general purpose buffer"
  (define buffer (buf (current-input-port) (lambda () '())))
  (and (buf-struct? buffer)
       (buf? buffer)))

; buf?
;------------------------------------------------------------------------------
(def-test "buf? should return true if passed a buffer struct with the right elements"
  (define buffer
    (make-buf
      (current-input-port)
      (lambda () '())
      0
      '()
      (vector)))
  (buf? buffer))

(def-test "buf? should return false if no load function"
  (define buffer
    (make-buf
      (current-input-port)
      #f ; <-- A procedure is required here
      0
      '()
      (vector)))
  (not (buf? buffer)))

(def-test "buf? should return false if pos is not an integer"
  (define buffer
    (make-buf
      (current-input-port)
      (lambda () '())
      "" ; <-- An integer required here
      '()
      (vector)))
  (not (buf? buffer)))

(def-test "buf? should return false if marks is not a list"
  (define buffer
    (make-buf
      (current-input-port)
      (lambda () '())
      0
      "" ; <-- a list required here
      (vector)))
  (not (buf? buffer)))

(def-test "buf? should return false if data is not a vector"
  (define buffer
    (make-buf
      (current-input-port)
      (lambda () '())
      0
      '()
      "" ; <-- A vector required here
      ))
  (not (buf? buffer)))

; buf-marked?
;------------------------------------------------------------------------------
(def-test "buf-marked? should return true if the buffer is marked"
  (define buffer
    (make-buf
      (current-input-port)
      (lambda () '())
      0
      '(0) ; Marks list is not empty
      (vector)))
  (buf-marked? buffer))


(def-test "buf-marked? should return false if the buffer is NOT marked"
  (define buffer
    (make-buf
      (current-input-port)
      (lambda () '())
      0
      '() ; Marks lists is empty
      (vector)))
  (not (buf-marked? buffer)))

; buf-mark!
;------------------------------------------------------------------------------
(def-test "buf-mark! should store the current pos in the marks list when the marks list is empty"
  (define buffer (buf (current-input-port) (lambda () '())))
  (buf-mark! buffer)
  (equal? '(0) (buf-marks buffer)))

(def-test "buf-mark! should prepend the current pos to the marks list"
  (define buffer (buf (current-input-port) (lambda () '())))
  (buf-marks-set! buffer '(1))
  (buf-mark! buffer)
  (equal? '(0 1) (buf-marks buffer)))

; buf-release!
;------------------------------------------------------------------------------
(def-test "buf-release! should remove the current mark from the marks list when only one mark exists"
  (define buffer (buf (current-input-port) (lambda () '())))
  (buf-marks-set! buffer '(0 1))
  (buf-release! buffer)
  (equal? '(1) (buf-marks buffer)))

(def-test "buf-release! should remove the current mark from the marks list when multiple marks exist"
  (define buffer (buf (current-input-port) (lambda () '())))
  (buf-marks-set! buffer '(0 1 2))
  (buf-release! buffer)
  (equal? '(1 2) (buf-marks buffer)))

; buf-advance!
;------------------------------------------------------------------------------
(def-test "buf-advance! should advance pos by 1"
  (define buffer (buf (current-input-port) (lambda () '())))
  (buf-advance! buffer)
  (= (buf-pos buffer) 1))

; buf-fill!
;------------------------------------------------------------------------------
(def-test "buf-fill! should load 0 items from the source"
  (call-with-input-string "abcdef"
    (lambda (input)
      (define buffer (buf input read-char))
      (buf-fill! buffer 0)
      (equal?
        (vector)
        (buf-data buffer)))))

(def-test "buf-fill! should load 1 items from the source"
  (call-with-input-string "abcdef"
    (lambda (input)
      (define buffer (buf input read-char))
      (buf-fill! buffer 1)
      (equal?
        (vector #\a)
        (buf-data buffer)))))

(def-test "buf-fill! should load 2 items from the source"
  (call-with-input-string "abcdef"
    (lambda (input)
      (define buffer (buf input read-char))
      (buf-fill! buffer 2)
      (equal?
        (vector #\a #\b)
        (buf-data buffer)))))

(def-test "buf-fill! should load 3 items from the source"
  (call-with-input-string "abcdef"
    (lambda (input)
      (define buffer (buf input read-char))
      (buf-fill! buffer 3)
      (equal?
        (vector #\a #\b #\c)
        (buf-data buffer)))))

; buf-sync!
;------------------------------------------------------------------------------
(def-test "buf-sync! should fill the buffer with the specified number of items when the buffer is empty"
  (call-with-input-string "abcdef"
    (lambda (input)
      (define buffer (buf input read-char))
      (buf-sync! buffer 3)
      (equal?
        (vector #\a #\b #\c)
        (buf-data buffer)))))

(def-test "buf-sync! should fill the buffer up to n when the buffer contains less data than required"
  (call-with-input-string "abcdef"
    (lambda (input)
      (define buffer (buf input read-char))
      (buf-data-set! buffer (vector #\z))
      (buf-sync! buffer 3)
      (equal?
        (vector #\z #\a #\b)
        (buf-data buffer)))))

(def-test "buf-sync! should do nothing if already synced"
  (call-with-input-string "abcdef"
    (lambda (input)
      (define buffer (buf input read-char))
      (buf-data-set! buffer (vector #\a #\b #\c))
      (buf-sync! buffer 3)
      (equal?
        (vector #\a #\b #\c)
        (buf-data buffer)))))

; buf-lookahead!
;------------------------------------------------------------------------------
(def-test "buf-lookahead! should return the first item of lookahead"
  (call-with-input-string "abcdef"
    (lambda (input)
      (define buffer (buf input read-char))
      (char=? #\a (buf-lookahead! buffer 1)))))

(def-test "buf-lookahead! should return the second item of lookahead"
  (call-with-input-string "abcdef"
    (lambda (input)
      (define buffer (buf input read-char))
      (char=? #\b (buf-lookahead! buffer 2)))))

(def-test "buf-lookahead! should return the third item of lookahead"
  (call-with-input-string "abcdef"
    (lambda (input)
      (define buffer (buf input read-char))
      (char=? #\c (buf-lookahead! buffer 3)))))

; buf-consume!
;------------------------------------------------------------------------------
(def-test "buf-consume! should clear the buffer if not marked and pos is equal to the buffer size"
  (call-with-input-string "abcdef"
    (lambda (input)
      (define buffer (buf input read-char))
      (buf-sync! buffer 3)
      (buf-pos-set! buffer 2)
      (and (char=? #\c (buf-consume! buffer))
           (= 0 (buf-pos buffer))
           (= 1 (vector-length (buf-data buffer)))))))

(def-test "buf-consume! should NOT clear the buffer if pos not equal to the buffer size"
  (call-with-input-string "abcdef"
    (lambda (input)
      (define buffer (buf input read-char))
      (buf-sync! buffer 3)
      (buf-pos-set! buffer 1)
      (and (char=? #\b (buf-consume! buffer))
           (= 2 (buf-pos buffer))
           (= 3 (vector-length (buf-data buffer)))))))

(def-test "buf-consume! should NOT clear the buffer if the buffer is marked"
  (call-with-input-string "abcdef"
    (lambda (input)
      (define buffer (buf input read-char))
      (buf-sync! buffer 3)
      (buf-pos-set! buffer 2)
      (buf-mark! buffer)
      (and (char=? #\c (buf-consume! buffer))
           (= 3 (buf-pos buffer))
           (= 4 (vector-length (buf-data buffer)))))))

