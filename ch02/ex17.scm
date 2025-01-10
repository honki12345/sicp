#lang sicp
(define (last-pair list1)
  (let ((car-value (car list1))
        (cdr-value (cdr list1)))
    (if (null? cdr-value)
        (list car-value)
        (last-pair cdr-value))))