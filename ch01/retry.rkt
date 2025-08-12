;; #lang sicp
#lang racket/base

10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b )))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))


(/ (+ 5 4 (- 2 (- 3 (+ 6
                      (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;; ex1.3
(define (ex1-3 x y z)
  (cond ((and (> x z) (> y z)) (+ (* x x) (* y y)))
        ((and (> y x) (> z x)) (+ (* y y) (* z z)))
        (else (+ (* x x) (* z z)))))

;; ex1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; ex1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     0.001))
(define (square x) (* x x))
(define (sqrt x)
  (sqrt-iter 1.0 x))

;; ex1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter2 (improve guess x)
                      x)))
(define (sqrt2 x) (sqrt-iter2 1.0 x))

;; ex1.8
(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (cube-improve guess x)
                 x)))

(define (cube-improve y x)
  (/ (+ (/ x
           (* y y))
        (* 2 y))
     3))

(define (cube-root x)
  (cube-iter 1.0 x))

;; ex1-9

;; ex1-10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; ex1.11
(define (ex1-11 n)
  (if (< n 3)
      n
      (+ (ex1-11 (- n 1))
         (* 2 (ex1-11 (- n 2)))
         (* 3 (ex1-11 (- n 3))))))

(define (ex1-11-2 n)
  (if (< n 3)
      n
      (ex1-11-2-iter 2 1 0 n)))

(define (ex1-11-2-iter a b c count)
  (if (= count 3)
      (+ a
         (* 2 b)
         (* 3 c))
      (ex1-11-2-iter (+ a
                        (* 2 b)
                        (* 3 c))
                     a
                     b
                     (- count 1))))

;; ex1-12
(define (ex1-12 row column)
  (cond ((= column 1) 1)
        ((= row column) 1)
        (else (+ (ex1-12 (- row 1) (- column 1))
                 (ex1-12 (- row 1) column)))))

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

;; ex1-16
(define (fast-expt2 b n)
  (fast-expt-iter b n 1))
(define (fast-expt-iter b n a)
   (if (= n 0)
       a
       (if (even? n)
           (fast-expt-iter (square b) (/ n 2) a)
           (fast-expt-iter b (- n 1) (* a b)))))

;; ex1-17
(define (*2 a b)
  (if (= b 0)
      0
      (+ a (*2 a (- b 1)))))
(define (double a) (* 2 a))
(define (halve a) (/ a 2))
(define (*3 a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (double (*3 a (halve b))))
        (else (+ a (*3 a (- b 1))))))

;; ex1-18
(define (*4 a b)
  (*4-iter a b 0))
(define (*4-iter a b sum)
  (cond ((= b 0) 0)
        ((= b 1) (+ a sum))
        ((even? b) (*4-iter (double a) (halve b) sum))
        (else (*4-iter a (- b 1) (+ a sum)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (divides? a b)
  (= (remainder b a) 0))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;; ex1-22
(define (runtime) ( current-inexact-milliseconds))
(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (prime?2 n)
  (= n (smallest-divisor2 n)))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (smallest-divisor2 n)
  (find-divisor2 n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (find-divisor2 n test-divisor)
  (define (next n)
    (if (= n 2)
        3
        (+ n 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor2 n (next test-divisor)))))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f))
(define (start-prime-test2 n start-time)
  (if (prime?2 n)
      (report-prime n (- (runtime) start-time))
      #f))
(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))
(define (search-for-primes a b)
  (cond ((> a b) (newline) (display "done"))
        ((even? a) (search-for-primes (+ a 1) b))
        (else (timed-prime-test a) (search-for-primes (+ a 2) b))))

(search-for-primes 1000 1020)
(search-for-primes 10000 10050)
(search-for-primes 100000 100045)
(search-for-primes 1000000 1000038)

;; ex1-23
(display "ex1-23")
(start-prime-test 1009 (runtime))
(start-prime-test2 1009 (runtime))
(start-prime-test 10007 (runtime))
(start-prime-test2 10007 (runtime))
(start-prime-test 100003 (runtime))
(start-prime-test2 100003 (runtime))


;; (define (sum-integers a b)
;;   (if (> a b)
;;       0
;;       (+ a (sum-integers (+ a 1) b))))

(define (cube a)
  (* a a a))

;; (define (sum-cubes a b)
;;   (if (> a b)
;;       0
;;       (+ (cube a) (sum-cubes (+ a 1) b))))

;; (define (pi-sum a b)
;;   (if (> a b)
;;       0
;;       (+ (/ 1.0 (* a (+ a 2)))
;;          (pi-sum (+ a 4) b))))


(define (sum-cubes a b)
  (sum cube a inc b))
(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; ex1.25

;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder (square (expmod base (/ exp 2) m))
;;                     m))
;;         (else
;;          (remainder (* base (expmod base (- exp 1) m))
;;                     m))))

;; (define (expmod base exp m)
;;   (remainder (fast-exp base exp) m))

;; (define (fast-expt b n)
;;   (cond ((= n 0) 1)
;;         ((even? n) (square (fast-expt b (/ n 2))))
;;         (else (* b (fast-expt b (- n 1))))))

;; ex1-26


;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder (square (expmod base (/ exp 2) m))
;;                     m))
;;         (else
;;          (remainder (* base (expmod base (- exp 1) m))
;;                     m))))


;; ex1.29

;; ex1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


;; ex1.31
(define (sum2 term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum2 term (next a) next b))))


(define (pi-product b)
  (define (pi-product-term a)
    (square
     (/ a (- a 1.0))))
  (define (pi-product-next a)
    (+ a 2))
  (* 8 (product pi-product-term 4 pi-product-next b)))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
;; 1.31 - b
(define (product2 term a next b)
  (define (product-iter a result)
    (if (> a b)
        result
        (product-iter (next a) (* (term a) result))))
  (product-iter a 1))

(define (inc a) (+ a 1))

(define (product-test a b)
  (product identity a inc b))
(define (product-test2 a b)
  (product2 identity a inc b))

;; ex1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum-accum term a next b)
  (accumulate + 0 term a next b))
(define (product-accum term a next b)
  (accumulate * 1 term a next b))

(define (sum-integers-accum a b)
  (sum-accum identity a inc b))
(define (product-integers-accum a b)
  (product-accum identity a inc b))

(define (accumulate-iter combiner null-value term a next b)
  (if (> a b)
      null-value
      (accumulate-iter combiner (combiner (term a) null-value) term a next b)))

(define (sum-integers-accum-iter a b)
  (sum-accum identity a inc b))
(define (product-integers-accum-iter a b)
  (product-accum identity a inc b))
(define (sum-accum-iter term a next b)
  (accumulate-iter + 0 term a next b))
(define (product-accum-iter term a next b)
  (accumulate-iter * 1 term a next b))

;; ex1.33

;; (define (accumulate combiner null-value term a next b)
;;   (if (> a b)
;;       null-value
;;       (combiner (term a)
;;                 (accumulate combiner null-value term (next a) next b))))
(define (filtered-accumulate predicate combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (predicate a)
          (combiner (term a)
                    (filtered-accumulate predicate combiner null-value term (next a) next b))
          (filtered-accumulate predicate combiner null-value term a next b))))


(define (pi-sum2 a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral2 f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

;; (define (f x y)
;;   (let ((a (+ 1 (* x y)))
;;         (b (- 1 y)))
;;     (+ (* x (square a))
;;        (* y b)
;;        (* a b))))



;; ex1-34

(define (f g)
  (g 2))
(f square)
(f (lambda (z) (* z (+ z 1))))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Value are not of opposite sign" a b)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt3 x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;; ex1-35
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;; ex1.36

(define (fixed-point2 f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (display "start fix-point")
  (newline)
  (try first-guess))

;; (fixed-point2 (lambda (x) (+ 1 (/ 1 x))) 1.0)
(fixed-point2 (lambda (x) (/ (log 1000) (log x))) 2.0)

;; ex1-37
(display " expected")
(/ 1 (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
(define (cont-frac k n d)
  (define (cont-frac-recurs n d count)
    (if (> count k)
        0
        (/ (n count)
           (+ (d count)
              (cont-frac-recurs n d (+ count 1))))))
  (cont-frac-recurs n
                    d
                    1))

(define (cont-frac2 k n d)
  (define (cont-frac-iter n d count value)
    (if (= count 0)
        value
        (cont-frac-iter n d (- count 1) (/ (n count)
                                           (+ (d count) value)))))
  (cont-frac-iter n
                  d
                  k
                  0))

(cont-frac 11 (lambda (i) 1.0) (lambda (i) 1.0))
(cont-frac2 11 (lambda (i) 1.0) (lambda (i) 1.0))

;; ex1-38
(display "ex1-38")
(+ 2 (cont-frac
      11
      (lambda (i) 1.0)
      (lambda (i)
        (cond ((= i 1) 1)
              ((= i 2) 2)
              (else (if (= (remainder i 3) 2)
                        (* 2 (/ (+ i 1) 3))
                        1))))))

;; ex1-39

;; (define (cont-frac k n d)
;;   (define (cont-frac-recurs n d count)
;;     (if (> count k)
;;         0
;;         (/ (n count)
;;            (+ (d count)
;;               (cont-frac-recurs n d (+ count 1))))))
;;   (cont-frac-recurs n
;;                     d
;;                     1))

(define (tan-cf x k)
  (define (recurs n d count)
    (if (> count k)
        0
        (/ (n count)
           (- (d count)
              (recurs n d (+ count 1))))))
  (recurs
   (lambda (i) (if (= i 1) x (square x)))
   (lambda (i) (- (* i 2) 1))
   1))

(define (average-dump f)
  (lambda (x) (average x (f x))))

(define (sqrt4 x)
  (fixed-point (average-dump (lambda (y) (/ x y)))
               1.0))
(define (cube-root2 x)
  (fixed-point (average-dump (lambda (y) (/ x (square y))))
               1.0))
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (sqrt5 x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

;; ex1-40

;; ex1-41
(define (double2 f)
  (lambda (x) (f (f x))))

;; ex1-42
(define (compose f g)
  (lambda (x) (f (g x))))
((compose square inc) 6)

;; ex1-43
(define (repeated f n)
  (if (> n 1)
      (compose f (repeated f (- n 1)))
      f))
((repeated square 2) 5)

;; ex1-44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx) (f x) (f (+ x dx))))
       3)))
(define (n-fold-smoothed f n)
  ((repeated smooth n) f))

;; ex1-46

(define (iterative-improve good-enough? improve)
  (define (recurs guess)
    (if (good-enough? guess)
        guess
        (recurs (improve guess))))
  (lambda (guess) (recurs guess)))

(define (iterative-improve2 good-enough? improve)
  (define (recurs guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
          next
          (recurs next))))
  (lambda (guess) (recurs guess)))

;; (define (sqrt-iter guess x)
;;   (if (good-enough? guess x)
;;       guess
;;       (sqrt-iter (improve guess x)
;;                  x)))

(define (sqrt-iterative-improve x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x))
       0.001))
  ((iterative-improve good-enough? improve) 1.0))

;; (define (fixed-point f first-guess)
;;   (define (close-enough? v1 v2)
;;     (< (abs (- v1 v2)) tolerance))
;;   (define (try guess)
;;     (let ((next (f guess)))
;;       (if (close-enough? guess next)
;;           next
;;           (try next))))
;;   (try first-guess))


(define (fixed-point-iterative-improve f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve2 close-enough? f) first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
(fixed-point-iterative-improve (lambda (x) (+ 1 (/ 1 x))) 1.0)

