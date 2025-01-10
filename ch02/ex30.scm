#lang sicp
(define tree (list 1 (list 2 (list 3 4) 5)
                   (list 6 7)))
(define (square x) (* x x))
;1
(define (square-tree tree square)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (square-tree (car tree) square)
                            (square-tree (cdr tree) square)))
        (else (square tree))))
;2
(define (square-tree2 tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree2 subtree)
             (square subtree)))
       tree))