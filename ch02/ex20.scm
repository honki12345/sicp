#lang sicp
(define (same-parity n . lst)
  (define (iter proc? lst new-lst)
    (if (null? lst)
        new-lst
        (if (proc? (car lst))
            (cons (car lst) (iter proc? (cdr lst) new-lst))
            (iter proc? (cdr lst) new-lst))))
  (if (= (remainder n 2) 0)
      (iter (lambda (x) (= (remainder x 2) 0)) (cons n lst) nil)
      (iter (lambda (x) (= (remainder x 2) 1)) (cons n lst) nil)))
      