#lang racket
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
; solution 1
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-up
           (transform-painter painter1
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0)))
          (paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point)))
      (lambda (frame)
        (paint-up frame)
        (paint-bottom frame)))))

;solution2
(define (below painter1 painter2)
  (rotate270 (beside (rotate270 (rotate270 (rotate270 painter1)))
                     (rotate270 (rotate270 (rotate270 painter2))))))