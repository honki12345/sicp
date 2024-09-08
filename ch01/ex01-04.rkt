#lang sicp
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; (a-plus-abs-b 4 9)
; ((if (> 9 0) + -) 4 9)
; (+ 4 9)
; 13