#lang sicp
(define set (list 1 2 3))
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (rest) (cons (car s) rest)) rest)))))
