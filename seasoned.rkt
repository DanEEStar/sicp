#lang racket

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (member? a lat)
  (cond ((null? lat) #f)
        (else (or (eq? a (car lat))
                  (member? a (cdr lat))))))

(define (two-in-a-row? lat)
  (cond ((null? lat) #f)
        (else (or (is-first? (car lat) (cdr lat))
                  (two-in-a-row? (cdr lat))))))

(define (is-first? a lat)
  (cond ((null? lat) #f)
        (else (eq? (car lat) a))))

(define (two-in-a-row2? lat)
  (cond ((null? lat) #f)
        (else (is-first-b? (car lat) (cdr lat)))))

(define (is-first-b? a lat)
  (cond ((null? lat) #f)
        (else (or (eq? (car lat) a)
                  (two-in-a-row2? lat)))))

(define (two-in-a-row-b? a lat)
  (cond ((null? lat) #f)
        (else (or (eq? a (car lat))
                  (two-in-a-row-b? (car lat) (cdr lat))))))

(define (sum-of-prefixes tup)
  (define (sum-of-prefixes-b ssf tup)
    (cond ((null? tup) '())
          (else
           (let ((newsum (+ (car tup) ssf)))
             (cons newsum
                   (sum-of-prefixes-b newsum (cdr tup)))))))
  (sum-of-prefixes-b 0 tup))

(define (pick n lat)
  (cond ((null? lat) null)
        ((= n 1) (car lat))
        (else (pick (- n 1) (cdr lat)))))

(define (scramble-b tup rev-pre)
  (printf "tup: ~a\n" tup)
  (printf "rev-pre: ~a\n" rev-pre)
  (cond ((null? tup) '())
        (else (cons (pick (car tup)
                          (cons (car tup) rev-pre))
                    (scramble-b (cdr tup) (cons (car tup) rev-pre))))))

(define (scramble tup)
  (scramble-b tup '()))

(define (multirember a lat)
  (define (mr lat)
    (cond ((null? lat) '())
          ((eq? a (car lat)) (mr (cdr lat)))
          (else (cons (car lat) (mr (cdr lat))))))
  (mr lat))

(define (multirember-f test?)
  (define (mr a lat)
    (define (mri lat)
      (cond ((null? lat) '())
            ((test? a (car lat)) (mri (cdr lat)))
            (else (cons (car lat) (mri (cdr lat))))))
    (mri lat))
  mr)

(define (intersect set1 set2)
  (cond ((null? set1) '())
        ((member? (car set1) set2)
         (cons (car set1) (intersect (cdr set1) set2)))
        (else (intersect (cdr set1) set2))))

(define (intersectall lset)
  (define (inner lset)
    (let/cc
     hop
     (printf "lset: ~a\n" lset)
     (cond ((null? (cdr lset)) (car lset))
           ((null? (car lset)) (hop '()))
           (else (intersect (car lset) (intersectall (cdr lset)))))))
  (if (null? lset) '()
      (inner lset)))

(define (rember-upto-last a lat)
  (let/cc skip
          (letrec
              ((R (lambda (lat)
                    (cond ((null? lat) '())
                          ((eq? (car lat) a) (skip (R (cdr lat))))
                          (else (cons (car lat) (R (cdr lat))))))))
            (R lat))))

(define (leftmost l)
  (cond
   ((null? l) '())
   ((atom? (car l)) (car l))
   (else (let ((a (leftmost (car l))))
           (if (atom? a)
               a
               (leftmost (cdr l)))))))
