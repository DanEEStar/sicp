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

; 1.29
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (inc x) (+ x 1))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((odd? k) 4)
             ((or (= k 0) (= k n)) 1)
             (else 2))
       (y k)
       (/ h 3)))
  (sum-iter term 0 inc n))

; 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (wallis-pi-product n)
  (define (term x)
    (if (even? x)
        (/ (+ x 2.0) (+ x 1.0))
        (/ (+ x 1.0) (+ x 2.0))))
  (acc-product term 1 inc n))

; 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (acc-product term a next b)
  (accumulate-iter * 1 term a next b))

(define (acc-sum term a next b)
  (accumulate-iter + 0 term a next b))

; ex 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a) (combiner result (term a)))
            (iter (next a) result))))
  (iter a null-value))

(define (filtered-sum term a next b filter)
  (filtered-accumulate + 0 term a next b filter))

(define (sum-prime-squares a b)
  (filtered-sum sqr a inc b exact-prime?))

; 1.35
(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

; 1.36
(define (fixed-point-debug f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt-debug x)
  (fixed-point-debug (lambda (y) (average y (/ x y))) 1.0))

; 1.37
(define (cont-frac n d k)
  (define (iter i)
    (cond ((= i k) (/ (n i) (d i)))
          ((= i 1) (/ (iter (+ i 1))))
          (else (+ (d (- i 1)) (/ (iter (+ i 1)))))))
  (iter 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 1)
        (/ (n i) result)
        (iter (- i 1) (+ (d (- i 1)) (/ (n i) result)))))
  (iter k 1))

(define (find-k-for-golden-ratio)
  (define tolerance 0.00005)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (one n) 1.0)
  (define (iter i)
    (if (close-enough? (/ 1.0 (cont-frac-iter one one i)) 1.618033988)
        i
        (iter (+ i 1))))
  (iter 1))

; 1.38
(define (euler-2 k)
  (define (one n) 1.0)
  (define (d n)
    (let ((nmod3 (modulo n 3))
          (dvalue (+ 2 (* (quotient n 3) 2))))
      (if (= nmod3 2)
          dvalue
          1.0)))
  (cont-frac-iter one d k))

; ex 1.39
(define (tan-cf x k)
  (define (cont-frac-iter-minus n d k)
    (define (iter i result)
      (if (= i 1)
          (/ (n i) result)
          (iter (- i 1) (- (d (- i 1)) (/ (n i) result)))))
    (iter k 1))
  (define (n y)
    (if (= y 1)
        x
        (sqr x)))
  (define (d y)
    (- (* y 2) 1))
  (cont-frac-iter-minus n d k))
