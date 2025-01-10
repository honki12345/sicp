#lang sicp
(define (reverse list1)
  (define (iter list1 reverse-list)
    (if (null? list1)
        reverse-list
        (iter (cdr list1) (cons (car list1) reverse-list))))
  (iter list1 nil))