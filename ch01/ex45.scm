#lang sicp
; fixed-point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; average-damp
(define (average a b)
  (/ (+ a b)
     2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

; repeated
(define (compose f g)
  (lambda (x) (f (g x))))
(define (square x) (* x x))
(define (identity x) x)
(define (repeated h count)
  (define (iter i)
    (if (> i count)
        identity
        (compose h
                 (iter (+ 1 i)))))
  (iter 1))
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(define (cubic x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y)))) 1.0))
(define (fourth x)
  (fixed-point (repeated (average-damp (lambda (y) (/ x (* y y y)))) 2) 1.0))
(define (five x)
  (fixed-point (repeated (average-damp (lambda (y) (/ x (* y y y y)))) 2) 1.0))
