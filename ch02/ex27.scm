#lang sicp
(define x (list (list 1 2) (list 3 4)))
(define (reverse list1)
  (define (iter list1 reverse-list)
    (if (null? list1)
        reverse-list
        (iter (cdr list1) (cons (car list1) reverse-list))))
  (iter list1 nil))
(define (deep-reverse list1)
  (define (iter list1 reverse-list)
    (if (null? list1)
        reverse-list
        (iter (cdr list1) (cons (if (pair? (car list1)) (deep-reverse (car list1)) (car list1)) reverse-list))))
  (iter list1 nil))

(define (deep-reverse2 list1)
  (define (iter list1 reverse-list)
    (cond ((null? list1) reverse-list)
          ((pair? (car list1)) (iter (cdr list1) (cons (deep-reverse (car list1)) reverse-list)))
          (else (iter (cdr list1) (cons (car list1) reverse-list)))))

  (iter list1 nil))

(deep-reverse x)
(deep-reverse2 x)
