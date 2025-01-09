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

(define dx 0.0001)
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx)))
                 3)))
(define (smooth-repeated n)
  (repeated smooth n))


         