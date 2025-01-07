#lang racket
(define (filtered-accumulate combiner null-value term a next b predicate?)
  (if (> a b)
      null-value
      (if (predicate? a)
          (combiner (term a)
                    (filtered-accumulate combiner null-value term (next a) next b predicate?))
          (filtered-accumulate combiner null-value term (next a) next b predicate?))))

(define (smallest-divisor n)
  (find-divisor 2 n))
(define (square x) (* x x))
(define (find-divisor test-divisor n)
  (if (> (square test-divisor) n)
      n
      (if (divides? test-divisor n)
          test-divisor
          (find-divisor (+ test-divisor 1) n))))
(define (divides? divisor n)
  (= (remainder n divisor) 0))
(define (prime? n)
  (= (smallest-divisor n) n))

;(define (filtered-accumulate combiner null-value term a next b predicate?)
(define (inc c) (+ 1 c))
(define (sum-prime a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (get-gcd a b)
  (if (= b 0)
      a
      (get-gcd b (remainder a b))))
(define (sum-gcd n)
  (define (gcd? a)
    (= 1 (get-gcd a n)))
  (filtered-accumulate * 1 identity 1 inc n gcd?))


