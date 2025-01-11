#lang sicp
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
;(A 1 10)
;1024
;(A 2 4)
;65536
;(A 3 3)
;65536

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
; (f n) -> 2n
(f 2)
(f 4)
(f 8)
; (g n) -> 2^n
(g 2)
(g 4)
; (h n) -> 2^2^2^2....2
(h 2)
(h 3)
(h 4)
(g 16)