(include "test.scm")
(declare (unit test_buf)
         (uses test))

; buf
;------------------------------------------------------------------------------
(def-test "buf should create a new general purpose buffer" '())

; buf-struct?
;------------------------------------------------------------------------------
(def-test "buf-struct? should return true if passed a buffer struct" '())
(def-test "buf-struct? should return fals if not a buffer struct" '())

; buf?
;------------------------------------------------------------------------------
(def-test "buf? should return true if passed a buffer struct with the right elements" '())
(def-test "buf? should return false if no load function" '())
(def-test "buf? should return false if pos is not an integer" '())
(def-test "buf? should return false if marks is not a list" '())
(def-test "buf? should return false if data is not a vector" '())

; buf-marked?
;------------------------------------------------------------------------------
(def-test "buf-marked? should return true if the buffer is marked" '())
(def-test "buf-marked? should return false if the buffer is NOT marked" '())

; buf-mark!
;------------------------------------------------------------------------------
(def-test "buf-mark! should store the current pos in the marks list when the marks list is empty" '())
(def-test "buf-mark! should prepend the current pos to the marks list" '())

; buf-release!
;------------------------------------------------------------------------------
(def-test "buf-release! should remove the current mark from the marks list when only one mark exists" '())
(def-test "buf-release! should remove the current mark from the marks list when multiple marks exist" '())

; buf-advance!
;------------------------------------------------------------------------------
(def-test "buf-advance! should advance pos by 1" '())

; buf-sync!
;------------------------------------------------------------------------------
(def-test "buf-sync! should fill the buffer with the specified number of bytes when the buffer is empty" '())
(def-test "buf-sync! should fill the buffer up to n when the buffer contains less data than required" '())
(def-test "buf-sync! should do nothing if already synced" '())

; buf-fill!
;------------------------------------------------------------------------------
(def-test "buf-fill! should load 0 bytes from the source" '())
(def-test "buf-fill! should load 1 bytes from the source" '())
(def-test "buf-fill! should load 2 bytes from the source" '())
(def-test "buf-fill! should load 3 bytes from the source" '())

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

