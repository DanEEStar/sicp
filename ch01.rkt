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

; sums
(define (cube x) (* x x x))

(define (my-sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (my-sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (my-sum (lambda (x) (* x x x)) a inc b))

(define (identity x) x)
(define (sum-integers a b)
  (my-sum identity a inc b))


(provide exact-prime?)
(provide fast-prime?)
(provide smallest-divisor)
(provide cube)
(provide inc)
(provide identity)
(provide my-sum)
(provide average)

; 1.3.3
(define (search f neg-point pos-point)
  (define (close-enough? x y) (< (abs (- x y)) 0.001))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))


(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt-fixed x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(provide fixed-point)

; 1.3.4
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt-damped x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (sqr y)))) 1.0))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (sqr y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-f1 x)
  (fixed-point-of-transform
   (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt-f2 x)
  (fixed-point-of-transform
   (lambda (y) (- (sqr y) x)) newton-transform 1.0))

(provide newtons-method)
(provide average-damp)
