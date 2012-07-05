(include "test.scm")
(declare (unit test_buf)
         (uses buf)
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
(def-test "buf-fill! should load 0 items from the source" '())
(def-test "buf-fill! should load 1 items from the source" '())
(def-test "buf-fill! should load 2 items from the source" '())
(def-test "buf-fill! should load 3 items from the source" '())

; buf-sync!
;------------------------------------------------------------------------------
(def-test "buf-sync! should fill the buffer with the specified number of items when the buffer is empty" '())
(def-test "buf-sync! should fill the buffer up to n when the buffer contains less data than required" '())
(def-test "buf-sync! should do nothing if already synced" '())

; buf-lookahead!
;------------------------------------------------------------------------------
(def-test "buf-lookahead! should return the first item of lookahead" '())
(def-test "buf-lookahead! should return the second item of lookahead" '())
(def-test "buf-lookahead! should return the third item of lookahead" '())

; buf-consume!
;------------------------------------------------------------------------------
(def-test "buf-consume! should clear the buffer if not marked and pos is equal to the buffer size" '())
(def-test "buf-consume! should NOT clear the buffer if pos not equal to the buffer size" '())
(def-test "buf-consume! should NOT clear the buffer if the buffer is marked" '())

