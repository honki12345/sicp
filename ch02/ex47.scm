#lang sicp
(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))
(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
; solutions
(define (origin-frame1 frame) (car frame))
(define (edge1-frame1 frame) (cadr frame))
(define (edge2-frame1 frame) (cadr (cdr frame)))
(define (origin-frame2 frame) (car frame))
(define (edge1-frame2 frame) (car (car (cdr frame))))
(define (edge2-frame2 frame) (cdr (car (cdr frame))))