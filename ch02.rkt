#lang racket

(require "ch01.rkt")

;2.1

;(define my-cons cons)
;(define my-car car)
;(define my-cdr cdr)

;(define (my-cons x y)
;  (define (dispatch m)
;    (cond ((= m 0) x)
;          ((= m 1) y)
;          (else (error "Argument not 0 or 1: CONS" m))))
;  dispatch)
;(define (my-car z) (z 0))
;(define (my-cdr z) (z 1))

; ex 2.4
(define (my-cons x y)
  (lambda (m) (m x y)))
(define (my-car z)
  (z (lambda (p q) p)))
(define (my-cdr z)
  (z (lambda (p q) q)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y)))) (define (div-rat x y)
                                        (make-rat (* (numer x) (denom y))
                                                  (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((numer (/ n g))
          (denom (/ d g)))
      (if (and (negative? denom) (positive? numer))
          (my-cons (- numer) (- denom))
          (my-cons numer denom)))))

(define (numer x) (my-car x))
(define (denom x) (my-cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (make-list n)
  (define (iter x result)
    (if (= x n)
        result
        (iter (+ x 1) (cons x result))))
  (iter 0 '()))

(define (my-append a b)
  (if (null? a)
      b
      (cons (car a) (append (cdr a) b))))

(define (atom? x) (not (or (pair? x) (null? x))))

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((atom? tree) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree2 sub-tree factor)
             (* sub-tree factor)))
       tree))

; 2.2.3 Sequences as Conventional Interfaces
(define (sum-odd-squares1 tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (sqr tree) 0))
        (else (+ (sum-odd-squares1 (car tree)) (sum-odd-squares1 (cdr tree))))))

(define (fib k)
  (define (iter a b n)
    (if (= n k)
        a
        (iter b (+ a b) (+ n 1))))
  (iter 1 1 0))

(define (even-fibs1 n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k))) (if (even? f)
                               (cons f (next (+ k 1)))
                               (next (+ k 1))))))
  (next 0))

(define (my-filter predicate seq)
  (cond ((null? seq) '())
        ((predicate (car seq)) (cons (car seq) (my-filter predicate (cdr seq))))
        (else (my-filter predicate (cdr seq)))))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(provide enumerate-interval)

(define (even-fibs2 n)
  (foldr cons '()
         (filter even?
                 (map fib
                      (enumerate-interval 0 n)))))

; Nested Mappings
(define (flatmap proc seq)
  (foldr append '() (map proc seq)))

(provide flatmap)

(define (prime-sum? pair)
  (exact-prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (printf "start: ~a\n" s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (printf "outer lambda: ~a\n" x)
                 (map (lambda (p)
                        (printf "inner lambda: ~a ~a\n" x p)
                        (cons x p))
                      (permutations (remove x s))))
               s)))
