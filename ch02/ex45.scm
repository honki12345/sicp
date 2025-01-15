#lang sicp
(#%require sicp-pict)
(define e einstein)
(define (up-split  painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

; solution
(define (split f1 f2)
  (define (helper painter n)
    (if (= n 0)
        painter
        (let ((smaller (helper painter (- n 1))))
          (f1 painter (f2 smaller smaller)))))
  helper)
(define right-split2 (split beside below))
(define up-split2 (split below beside))
; test
(paint (right-split e 4))
(paint (right-split2 e 4))
(paint (up-split e 4))
(paint (up-split2 e 4))