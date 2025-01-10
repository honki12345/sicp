#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (count-leaves t)
  (accumulate +
              0
              (map
               (lambda (x)
                 (cond ((not (pair? x)) 1)
                       ((and (pair? (car x)) (pair? (cdr x)))
                        (+ (count-leaves (car x))
                           (count-leaves (cdr x))))
                       ((pair? (car x)) (+ 1 (count-leaves (car x))))
                       ((pair? (cdr x)) (+ 1 (count-leaves (cdr x))))
                       (else 2)))
               t)))