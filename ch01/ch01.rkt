#lang racket

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
