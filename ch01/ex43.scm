#lang sicp
(define (compose f g)
  (lambda (x) (f (g x))))
(define (square x) (* x x))
(define (identity x) x)
(define (repeated h count)
  (define (iter i)
    (if (> i count)
        identity
        (compose h
                 (iter (+ 1 i)))))
  (iter 1))