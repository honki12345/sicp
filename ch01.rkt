#lang sicp
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (square x) (* x x))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
;(sqrt 9)
;(sqrt (+ 100 37))
;(sqrt (+ (sqrt 2) (sqrt 3)))
;(square (sqrt 1000))

; ex1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
;(new-if (= 2 3) 0 5)
;(new-if (= 1 1) 0 5)
(define (sqrt-iter-alyssa guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-alyssa (improve guess x)
                     x)))

;(sqrt-iter-alyssa 1.0 3)
;(sqrt-iter-alyssa 1.0 9)
;(sqrt-iter 1.0 3)
;(sqrt-iter 1.0 9)

; ex1.7
;(sqrt 0.1)
;(sqrt 0.01)
;(sqrt 0.001)
;(sqrt 0.0001)
;(sqrt 0.00001)
; 
;(sqrt 0.000001)
;(sqrt 0.0000001)
;(sqrt 0.00000001)

(define (new-good-enough? previous-guess guess)
  (< (abs (/ (- guess previous-guess) guess)) 0.000000000000001))

(define (sqrt-iter-improved guess x)
  (if (new-good-enough? guess (improve guess x))
      guess
      (sqrt-iter-improved (improve guess x) x)))
(define (sqrt-improved x)
  (sqrt-iter-improved 1.0 x))
;(sqrt 0.00001)
;(sqrt-improved 0.00001)


; ex1.8
(define (cube-root-iter guess x)
  (define (good-enough? previous-guess guess)
    (< (abs (/ (- guess previous-guess) guess)) 0.000000001))
  (define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess))
       3))
  (if (good-enough? guess (improve guess x))
      guess
      (cube-root-iter (improve guess x) x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))
;cube-root 27)

(define (sqrt2 x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
;(sqrt2 4)

; 1.2.1
; if test-expr then-expr else-expr)
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
;(factorial 6)
(define (factorial2 n)
  (define (fact-iter product counter)
    (if (> counter n)
        product
        (fact-iter (* product counter) (+ counter 1))))
  (fact-iter 1 1))
;(factorial2 6)

; ex 1.9
(define (plus1 a b)
  (if (= a 0)
      b
      (inc (plus1 (dec a) b))))
; (plus1 4 5)
; (inc (plus1 3 5))
; (inc (inc (plus1 2 5)))
; (inc (inc (inc (plus1 1 5))))
; (inc (inc (inc (inc (plus 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

(define (plus2 a b)
  (if (= a 0)
      b
      (plus2 (dec a) (inc b))))
; (plus2 4 5)
; (plus2 3 6)
; (plus2 2 7)
; (plus2 1 8)
; (plus2 0 9)
; 9

;(plus1 4 5)
;(plus2 4 5)


; ex 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
;(A 1 10)
;(A 2 4)
;(A 3 3)
(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))


; 1.2.2
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib2 n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))
;(fib 5)
;(fib2 5)

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
(count-change 100)

; 1.2.4
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt2 b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b
                   (- counter 1)
                   (* b product))))
  (expt-iter b n 1))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))


; 1.2.5
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; 1.2.6
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
;(prime? 2)
;(prime? 4)

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
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
















 