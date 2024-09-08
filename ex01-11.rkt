#lang sicp
; ex 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2
            (f (- n 2)))
         (* 3
            (f (- n 3))))))
(f 3)
(define (f2 n)
  (define (f-iter counter total minus1 minus2 minus3)
    (if (>= counter n)
        total
        (f-iter (+ counter 1)
                (+ total (* 2 minus1) (* 3 minus2))
                total
                 minus1
                 minus2)))
    (if (< n 3)
      n
      (f-iter 3 4 2 1 0)))
(f2 3)
(f 0)
(f2 0)
(f 5)
(f2 5)
              
    
  