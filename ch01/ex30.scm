#lang racket
(define (sum2 term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;test
(define (inc n) (+ n 1))
(define (cube x)
  (* x x x))
(define (sum-cubes a b)
  (sum cube a inc b))
(define (sum-cubes2 a b)
  (sum2 cube a inc b))
