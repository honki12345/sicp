#lang sicp
(define (even? n)
  (= (remainder n 2) 0))
(define (fast-expt b n)
  (define (fast-expt-iter value counter)
    (cond ((= counter n) value)
          ((< (* counter 2) n) (fast-expt-iter (* value value) (* 2 counter)))
          (else (fast-expt-iter (* value b) (+ 1 counter)))))
  (if (= n 0)
      1
      (fast-expt-iter b 1)))
(fast-expt 2 3)
(fast-expt 3 4)
