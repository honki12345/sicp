#lang racket/base

;; pair

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

(define x (cons 1 2))

;; ex2-1

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((and (< n 0) (> d 0))
           (cons (/ n (abs g)) (/ d g)))
          ((and (> n 0) (< d 0))
           (cons (/ n g) (/ d g)))
          (else (cons (/ n g) (/ d g))))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; ex2-2
(define (make-segment s e)
  (cons s e))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))
(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment))
                      (x-point (end-segment segment))) 2)
              (/ (+ (y-point (start-segment segment))
                      (y-point (end-segment segment))) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define p1 (make-point 3 5))
(define p2 (make-point 1 9))
(define seg (make-segment p1 p2))
(print-point (midpoint-segment seg))

;; ex2-4
;; (define (cons x y)
;;   (lambda (m) (m x y)))
;; (define (car z)
;;   (z (lambda (p q) p)))
;; (define (cdr z)
;;   (z (lambda (p q) q)))

;; ex2-6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f (n f) x))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; ex2-7
(define (make-interval a b)
  (cons a b))
(define (upper-bound interval)
  (car interval))
(define (lower-bound interval)
  (cdr interval))

;; ex2-8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define one-through-four (list 1 2 3 4))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; ex2-17
(define (last-pair items)
  (list (list-ref items (- (length items) 1))))

;; ex2-18
(define (reverse items)
  (if (= (length items) 1)
      (list (car items))
      (append (reverse (cdr items)) (list (car items)))))


;; ex2-19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (define (no-more? coin-values) (null? coin-values))
  (define (except-first-denomination coin-values)
    (cdr coin-values))
  (define (first-denomination coin-values)
    (car coin-values))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;; ex2-20
(define (same-parity . items)
  (define (iter items first-item)
    (if (null? items)
        null
        (if (= (remainder (car items) 2) (remainder first-item 2))
            (cons (car items) (iter (cdr items) first-item))
            (iter (cdr items) first-item))))
  (iter items (car items)))
