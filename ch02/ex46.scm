#lang sicp
;solution
(define (make-vect x y)
  (list x y))
(define (xcor-vect vec)
  (car vec))
(define (ycor-vect vec)
  (cadr vec))
(define (add-vect vec1 vec2)
  (make-vect (+ (xcor-vect vec1)
                (xcor-vect vec2))
             (+ (ycor-vect vec1)
                (ycor-vect vec2))))
(define (sub-vect vec1 vec2)
  (make-vect (- (xcor-vect vec1)
                (xcor-vect vec2))
             (- (ycor-vect vec1)
                (ycor-vect vec2))))
(define (scale-vect n vect)
  (make-vect (* n (xcor-vect vect))
             (* n (ycor-vect vect))))


