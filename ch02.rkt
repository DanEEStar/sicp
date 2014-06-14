#lang racket
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
