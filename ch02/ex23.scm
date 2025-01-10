#lang sicp
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
(define (for-each proc items)
  (map proc items)
  (newline))