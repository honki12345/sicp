#lang sicp
(define (square x) (* x x))
(square 5)
(define (solution x y z) (cond
                           ((and (> x z) (> z y)) (+ (square x) (square z)))
                           ((and (> x y) (> y z)) (+ (square x) (square y)))
                           ((and (> y x) (> x z)) (+ (square y) (square x)))
                           ((and (> y z) (> z x)) (+ (square y) (square z)))
                           ((and (> z x) (> x y)) (+ (square z) (square x)))
                           ((and (> z y) (> y x)) (+ (square z) (square y)))))
(solution 3 4 5)

