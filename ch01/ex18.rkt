#lang sicp
(define (* a b)
  (multi-iter 0 a b))
(define (double x)
  (+ x x))
(define (halve x)
  (/ x 2))
(define (even? x)
  (= (remainder x 2) 0))
(define (multi-iter n a b)
  (cond ((= b 0) n)
        ((even? b) (multi-iter n (double a) (halve b)))
        (else (multi-iter (+ n a) a (- b 1)))))
        