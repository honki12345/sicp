#lang sicp
(define (square x) (* x x))
; x y z 중 x, y 가 큰 경우, y, z 가 큰 경우, x, z가 큰 경우 세 개로 나눌 수 있다
(define (square-bigger x y z)
  (cond ((and (>= x z) (>= y z)) (+ (square x) (square y)))
        ((and (>= y x) (>= z x)) (+ (square y) (square z)))
        (else (+ (square x) (square z)))))
(square-bigger 3 4 5)