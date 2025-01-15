#lang sicp


;solution
(define (adjoin-set x set)
  (if (null? set)
      (cons x set)
      (let ((car-set (car set)))
        (cond ((= x car-set) set)
              ((< x car-set) (cons x set))
              (else (cons car-set (adjoin-set x (cdr set))))))))
          