#lang sicp

; ex1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
;(new-if (= 2 3) 0 5)
;(new-if (= 1 1) 0 5)
(define (sqrt-iter-alyssa guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-alyssa (improve guess x)
                     x)))

;(sqrt-iter-alyssa 1.0 3)
;(sqrt-iter-alyssa 1.0 9)
;(sqrt-iter 1.0 3)
;(sqrt-iter 1.0 9)