(declare (unit charport))
;(define (charport-read chprt)
;  (define ch (read-char (charport-port chprt)))
;  (cond
;    ((eof-object? ch)) ; Do nothing for EOFs
;    ((char=? ch #\newline)
;      (charport-line-set! chprt (+ 1 (charport-line chprt)))
;      (charport-column-set! chprt 1))
;    (else
;      (charport-column-set! chprt (+ 1 (charport-column chprt)))))
;  (charobj ch (charport-posdata chprt))
;
;(define (charport-posdata chprt)
;  (posdata
;    (port-name (charport-port chprt))
;    (charport-line chprt)
;    (charport-column chprt)))


