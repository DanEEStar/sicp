#lang racket

(require math)

; sqrt
(define (square x) (* x x))

(define (average a b) (/ (+ a b) 2))

(define (sqrt x)
  (define (good-enough? guess lastguess)
    (< (abs (- guess lastguess)) 0.00001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess lastguess)
    (if (good-enough? guess lastguess)
        guess
        (sqrt-iter (improve guess) guess)))
  (sqrt-iter 1.0 0.0))

; count change
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; smallest divisors
(define (next-for-find-divisor n) (+ n 1))
(define (smallest-divisor n next-func) (find-divisor n 2 next-func))

(define (find-divisor n test-divisor next-func)
  (cond ((> (sqr test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-func test-divisor) next-func))))

(define (exact-prime? n)
  (= n (smallest-divisor n next-for-find-divisor)))

; fermat stuff
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (sqr (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-natural (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(provide exact-prime?)
(provide fast-prime?)
(provide smallest-divisor)
