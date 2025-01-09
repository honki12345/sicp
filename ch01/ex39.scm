#lang sicp
(define (tan-cf x k)
  (define (iter i)
    (if (> i k)
        0
        (/ (if (= i 1) x (* x x))
           (- (- (* 2 i) 1)
              (iter (+ i 1))))))
  (iter 1))
