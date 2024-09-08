#lang sicp
; ex 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

(define (hA 2 n)
  (cond ((= n 0) 0)
        ((= 2 0) (* 2 n)) ; never
        ((= n 1) 2)
        (else (A 1 (A 2 (- n 1))))))


(define (gA 1 n)
  (cond ((= n 0) 0)
        ((= 1 0) (* 2 n)) ; never
        ((= n 1) 2)
        ;(else (A 0 (A 1 (- n 1))))))
        (else (* 2 (gA 1 (- n 1))))))
; g(n) = 2 ^ n
; g(n) = 2 * g(n - 1)
; g(0) = 1
; g(1) = 2


(define (fA 0 n)
  (cond ((= n 0) 0)
        ((= 0 0) (* 2 n)) ; Always true
        ((= n 1) 2)        ; never executed because (= 0 0) is always true
        (else (A (- 0 1)   ; never executed
                 (A 0 (- n 1))))))
(define (simpleA 0 n)
  (* 2 n))


  









