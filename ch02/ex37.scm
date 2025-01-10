#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
; 1
(define (matrix-*-vector m v)
  (map (lambda (x) (accumulate + 0 (map * x v))) m))

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9) (list 3 3 3 3)))
(define v (list 1 1 1 1))

; 2
(define (transpose mat)
  (accumulate-n cons nil mat))

; 3
(define (matrix-*-matrix m n)
  (let ((clos (transpose n)))
    (map (lambda (x)
           (map (lambda (y) (accumulate + 0 (map * y x)))
                clos))
         m)))




