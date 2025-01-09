#lang sicp
; recursive
(define (cont-frac n d k)
  (define (iter start)
    (let ((n-value (n start))
          (d-value (d start)))
      (if (> start k)
          0
          (/ n-value
             (+ d-value
                (iter (+ start 1)))))))
  (iter 1))


(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)

; iter
(define (cont-frac2 n d k)
  (define (iter k total)
    (if (= k 0)
        total
        (iter (- k 1)
              (/ (n k)
                 (+ (d k)
                    total)))))
  (iter k 0))
(cont-frac2 (lambda (i) 1.0)
            (lambda (i) 1.0)
           12)