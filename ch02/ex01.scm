#lang sicp
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (abs (gcd n d)))
        (abs-n (abs n))
        (abs-d (abs d)))
    (if (< (* n d) 0)
        (cons (/ (if (< n 0) n (* n -1))
                 g)
              (/ abs-d g))
        (cons (/ abs-n g)
              (/ abs-d g)))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))






