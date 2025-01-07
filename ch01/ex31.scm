#lang racket
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
;test
(define (identity x) x)
(define (inc x) (+ x 1))
(define (product-integers a b)
  (product identity a inc b))

(define (get-even x)
  (* (* 2.0 x)
     (* 2.0 x)))
(define (get-odd x)
  (* (+ 1 (* 2.0 x))
     (+ 1 (* 2.0 x))))
(define (get-pi a b)
  (* 2
     (/ (product get-even a inc b)
        (product get-odd a inc b))))
; 모르겠다