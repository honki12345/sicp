#lang sicp

; ex1.7
;(sqrt 0.1)
;(sqrt 0.01)
;(sqrt 0.001)
;(sqrt 0.0001)
;(sqrt 0.00001)
; 
;(sqrt 0.000001)
;(sqrt 0.0000001)
;(sqrt 0.00000001)

(define (new-good-enough? previous-guess guess)
  (< (abs (/ (- guess previous-guess) guess)) 0.000000000000001))

(define (sqrt-iter-improved guess x)
  (if (new-good-enough? guess (improve guess x))
      guess
      (sqrt-iter-improved (improve guess x) x)))
(define (sqrt-improved x)
  (sqrt-iter-improved 1.0 x))
;(sqrt 0.00001)
;(sqrt-improved 0.00001)
