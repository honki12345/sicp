#lang racket
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
(define (identity x) x)
(define (inc x) (+ 1 x))
(define (sum-accumulate a b)
  (accumulate + 0 identity a inc b))
(define (product-accumulate a b)
  (accumulate * 1 identity a inc b))

;b
(define (accumulate2 combiner null-value term a next b)
  (define (accumulate-iter a total)
    (if (> a b)
        total
        (accumulate-iter (next a) (combiner total a))))
  (accumulate-iter a null-value))

(define (sum-accumulate2 a b)
  (accumulate2 + 0 identity a inc b))





     
  
