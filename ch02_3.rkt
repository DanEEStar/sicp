#lang racket

(require "ch02.rkt")

; Chapter 2.3 Symbolic Data

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; ex 2.54
(define (my-equal? s1 s2)
  (cond ((eq? s1 s2) #t)
        ((and (and (pair? s1)
                   (pair? s2))
              (eq? (car s1) (car s2)))
         (my-equal? (cdr s1) (cdr s2)))
        (else #f)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentation (base exp)
                                           (make-sum (exponent exp) -1))))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))


(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s)
  (let ((rest (cddr s)))
    (if (null? (cdr rest))
        (car rest)
        rest)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p)
  (let ((rest (cddr p)))
    (if (null? (cdr rest))
        (car rest)
        rest)))

; ex 2.56
(define (make-exponentation u n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) u)
        (else (list '** u n))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

; 2.3.3
; sets as unordered lists
(define (element-of-set-uo? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set-uo? x (cdr set)))))

(define (adjoin-set-uo x set)
  (if (element-of-set-uo? x set)
      set
      (cons x set)))

(define (intersection-set-uo set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set-uo? (car set1) set2)
         (cons (car set1) (intersection-set-uo (cdr set1) set2)))
        (else (intersection-set-uo (cdr set1) set2))))

(define (union-set-uo set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set-uo? (car set1) set2)
         (union-set-uo (cdr set1) set2))
        (else (cons (car set1) (union-set-uo (cdr set1) set2)))))

; set as ordered lists
(define (element-of-set-s? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set-s? x (cdr set)))))

(define (intersection-set-s set1 set2)
  (if (or (null? set1) (null? set2)) '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2) (cons x1 (intersection-set-s (cdr set1)
                                                      (cdr set2))))
              ((< x1 x2)
               (intersection-set-s (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-s set1 (cdr set2)))))))

; ex 2.61
(define (adjoin-set-s x set)
  (if (null? set)
      (list x)
      (let ((y (car set)))
        (cond ((= x y) set)
              ((< x y) (cons x set))
              ((> x y) (cons y (adjoin-set-s x (cdr set))))))))

; ex 2.62
(define (union-set-s set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set-s (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set-s (cdr set1) set2)))
                 ((< x2 x1)
                  (cons x2 (union-set-s set1 (cdr set2)))))))))

; sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-b? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set-b? x (left-branch set)))
        ((> x (entry set))
         (element-of-set-b? x (right-branch set)))))

(define (adjoin-set-b x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set-b x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set-b x (right-branch set))))))

; ex 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define testtree1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

(define testtree2
  (make-tree 3
             (make-tree 1 '() '())
             (make-tree 7
                        (make-tree 5 '() '())
                        (make-tree 9
                                   '()
                                   (make-tree 11 '() '())))))

(define testtree3
  (make-tree 5
             (make-tree 3
                        (make-tree 1 '() '())
                        '())
             (make-tree 9
                        (make-tree 7 '() '())
                        (make-tree 11 '() '()))))
