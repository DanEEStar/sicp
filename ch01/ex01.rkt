#lang racket

(require "ch01.rkt")

; ex 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))


; ex 1.11
(define (f1 n)
  (if (< n 3)
      n
      (+ (f1 (- n 1)) (f1 (- n 2)) (f1 (- n 3)))))

(define (fiter n a b c)
  (if (= n 0)
      c
      (fiter (- n 1) (+ a b c) a b)))

(define (f2 n) (fiter n 2 1 0))


; ex 1.16
(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (sqr b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* b a)))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))


; ex 1.17
(define (double x) (* 2 x))

(define (halve x) (/ x 2))

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mul a (halve b))))
        (else (+ a (fast-mul a (- b 1))))))

; ex 1.18
(define (fast-mul-iter a b x)
  (cond ((= b 0) x)
        ((even? b) (fast-mul-iter (double a) (halve b) x))
        (else (fast-mul-iter a (- b 1) (+ a x)))))
(define (fast-mul2 a b)
  (fast-mul-iter a b 0))

; ex 1.19 fib in log time
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (sqr p) (sqr q))
                   (+ (* 2 p q) (sqr q))
                   (halve count)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; ex 1.22
(define (search-for-primes start counter prime?)
  (cond ((= counter 0) '())
        ((prime? start) (cons start
                                    (search-for-primes
                                     (+ start 1)
                                     (- counter 1)
                                     prime?)))
        (else (search-for-primes (+ start 1) counter prime?))))

; ex 1.23
(define (my-next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (my-prime? n)
  (= n (smallest-divisor n my-next)))

; ex 1.24
(define (my-fast-prime? n)
  (fast-prime? n 100))