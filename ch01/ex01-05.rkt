#lang sicp
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
(test 0 (p))
;인자먼저 계산
; (test 0 (p))
; (test 0 (p))
; (test 0 (p))
; 정의대로
; (test 0 (p))
; (if (= 0 0) 0 (p))
; (if #t 0 (p))
