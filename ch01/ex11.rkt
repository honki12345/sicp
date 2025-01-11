#lang sicp
(define (solution n)
  (cond ((< n 3) n)
        (else (+ (solution (- n 1))
                 (* 2 (solution (- n 2)))
                 (* 3 (solution (- n 3)))))))

(solution 3)
(solution 4)
(solution 5)
(solution 6)
(solution 7)
(solution 8)

(define (solution2 n)
  (define (iter a b c d e count)
    (cond ((= n count) (+ a (* 2 b) (* 3 c)))
          (else (iter
                  (+ a (* 2 b) (* 3 c))
                  a
                  b
                  c
                  d
                  (+ 1 count)))))
  (cond ((< n 3) n)
        ((= n 3) 4)
        ((= n 4) 11)
        ((= n 5) 25)
        (else (iter 25 11 4 3 2 6))))


(solution2 3)
(solution2 4)
(solution2 5)
(solution2 6)
(solution2 7)
(solution2 8)