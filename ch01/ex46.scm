#lang sicp
(define (iterative-improve enough? improve)
  (define (iter guess)
    (if (enough? (improve guess))
        (improve guess)
        (iter (improve guess))))
  (lambda (guess) (iter guess)))
(define (average a b) (/ (+ a b) 2.0))
(define (square x) (* x x))
; sqrt
(define (sqrt x)
  ((iterative-improve
   (lambda (guess) (< (abs (- (square guess) x)) 0.0001))
   (lambda (guess) (average guess (/ x guess)))) x))
; fixed-point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (guess) (< (abs (- guess (f guess))) tolerance))
    f) first-guess))


     