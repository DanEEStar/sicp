#lang racket

(define (average a b) (/ (+ a b) 2))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))
(define (midpoint-segment s)
  (make-point (average (x-point (start-segment s))
                       (x-point (end-segment s)))
              (average (y-point (start-segment s))
                       (y-point (end-segment s)))))

; ex 2.5
(define (factor-out n factor)
  (if (> (remainder n factor) 0)
      n
      (factor-out (/ n factor) factor)))

(define (nth-root n base)
  (define (iter n res)
    (if (= n base)
        res
        (iter (/ n base) (+ res 1))))
  (iter n 1))

(define (integer-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (integer-car x)
  (nth-root (factor-out x 3) 2))

(define (integer-cdr x)
  (nth-root (factor-out x 2) 3))

; ex 2.6
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define (zero f) (lambda (x) x))
(define (one f) (lambda (x) (f x)))
(define (two f) (lambda (x) (f (f x))))
(define (three f) (lambda (x) (f (f (f x)))))
(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
(define (mult a b)
  (lambda (f) (a (b f))))

(define (inc n)
  (+ n 1))
