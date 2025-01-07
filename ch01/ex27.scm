#lang sicp
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (square x) (* x x))
(define (fermat-test? n)
  (define (iter start)
    (or (>= start n)
        (and (= (expmod start n n) start)
             (iter (+ start 1)))))
  (iter 1))










  