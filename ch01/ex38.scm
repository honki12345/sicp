#lang sicp
(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i)
              (iter (+ 1 i))))))
  (iter 1))
(define (euler-value k)
  (+ 2
     (cont-frac (lambda (i) 1.0)
           (lambda (i)
             (if (= (remainder (+ i 1) 3) 0)
                 (* 2 (/ (+ i 1)
                         3))
                 1))
           k)))