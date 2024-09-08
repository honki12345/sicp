#lang sicp

; ex1.8
(define (cube-root-iter guess x)
  (define (good-enough? previous-guess guess)
    (< (abs (/ (- guess previous-guess) guess)) 0.000000001))
  (define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess))
       3))
  (if (good-enough? guess (improve guess x))
      guess
      (cube-root-iter (improve guess x) x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))
;cube-root 27)

(define (sqrt2 x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
;(sqrt2 4)
