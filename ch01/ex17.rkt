#lang sicp
(define (* a b)
  (if (= b 0)
      0
      (+ a
         (* a
            (- b 1)))))
(define (double x)
  (+ x x))
(define (halve x)
  (/ x 2))
(define (even? x)
  (= (remainder x 2) 0))
(define (multi-fast a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (multi-fast (double a) (halve b)))
        (else (+ a (multi-fast a (- b 1))))))

        