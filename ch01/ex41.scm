#lang sicp
(define (double f)
  (lambda (x) (f (f x))))


(((double (double double))
  (lambda (x) (+ 1 x)))
 5)