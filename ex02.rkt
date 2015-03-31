#lang racket

(require "ch02.rkt")

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

; extended exercise 2.1.4
(define (make-interval a b)
  (cons a b))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (width-interval x)
  (- (upper-bound x) (lower-bound x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (let ((upper-y (upper-bound y))
        (lower-y (lower-bound y)))
    (if (and (<= lower-y 0) (>= upper-y 0))
        (error "div interval spans zero...")
        (mul-interval
         x
         (make-interval (/ 1.0 upper-y)
                        (/ 1.0 lower-y))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))
(define (percent x)
  (/ (width x) (center x)))

; ex 2.17
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; ex 2.18
(define (my-reverse x)
  (define (iter x result)
    (if (null? x)
        result
        (iter (cdr x) (cons (car x) result))))
  (iter x '()))

; ex 2.19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination
                      coin-values))
                 (cc (- amount
                        (first-denomination
                         coin-values))
                     coin-values)))))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(define us-coins '(50 25 10 5 1))
(define uk-coins '(100 50 20 10 5 2 1 0.5))
(define swiss-coins '(500 200 100 50 20 10 5))

; ex 2.20
(define (same-parity n . w)
  (define (iter w)
    (cond ((null? w) '())
          ((= (modulo n 2) (modulo (car w) 2))
           (cons (car w) (iter (cdr w))))
          (else (iter (cdr w)))))
  (iter (cons n w)))

; ex 2.21
(define (square-list1 items)
  (if (null? items)
      '()
      (cons (sqr (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  (map sqr items))

; ex 2.22
(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (sqr (car things))
                    answer))))
  (iter items '()))

; ex 2.23
(define (for-each proc items)
  (cond ((null? items) #true)
        (else (proc (car items)) (for-each proc (cdr items)))))

; ex 2.24
(define ex224_1 (list 1 (list 2 (list 3 4))))
; is equal to
(define ex224_2 (cons 1 (cons (cons 2 (cons (cons 3 (cons 4 '())) '())) '())))

; 2.27
(define (atom? x) (not (or (pair? x) (null? x))))
(define (deep-reverse tree)
  (define (iter x result)
    (cond ((null? x) result)
          ((pair? (car x))
           (iter (cdr x) (cons (deep-reverse (car x)) result)))
          (else (iter (cdr x) (cons (car x) result)))))
  (iter tree '()))

; 2.28
(define (fringe tree)
  (cond ((null? tree) '())
        ((pair? (car tree)) (append (fringe (car tree)) (fringe (cdr tree))))
        (else (cons (car tree) (fringe (cdr tree))))))

(define (fringe2 tree)
  (cond ((null? tree) '())
        ((atom? tree) (list tree))
        (else (append (fringe2 (car tree)) (fringe2 (cdr tree))))))

; ex 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))


(define (weight-for-branch branch)
  (if (number? (branch-structure branch))
      (branch-structure branch)
      (weight-for-mobile (branch-structure branch))))
(define (weight-for-mobile mobile)
  (+ (weight-for-branch (left-branch mobile))
     (weight-for-branch (right-branch mobile))))
(define (total-weight mobile)
  (weight-for-mobile mobile))

(define (mobile-balanced? mobile)
  (define (branch-torque branch)
    (* (branch-length branch) (weight-for-branch branch)))
  (define (branch-balanced? branch)
    (if (pair? (branch-structure branch))
        (mobile-balanced? (branch-structure branch))
        #t))
  (and (= (branch-torque (left-branch mobile))
          (branch-torque (right-branch mobile)))
       (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))))

; ex 2.30
(define test-tree '(1 (2 (3 4) 5) (6 7)))
(define (square-tree1 tree)
  (cond ((null? tree) '())
        ((atom? tree) (sqr tree))
        (else (cons (square-tree1 (car tree))
                    (square-tree1 (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree2 x)
             (sqr x)))
       tree))

; ex 2.31
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))
(define (square-tree3 tree)
  (tree-map sqr tree))

; ex 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; ex 2.33
(define (acc-map p sequence)
  (foldr (lambda (x y) (cons (p x) y)) '() sequence))

(define (acc-append seq1 seq2)
  (foldr cons seq2 seq1))

(define (acc-length sequence)
  (foldr (lambda (x y)
           (+ y 1)) 0 sequence))

(define (acc-filter predicate sequence)
  (foldr (lambda (x y)
           (if (predicate x)
               (cons x y)
               y))
         '()
         sequence))

; ex 2.34
(define (horner-eval x coeff-seq)
  (foldr (lambda (this-coeff higher-terms)
           (+ (* x higher-terms) this-coeff))
         0
         coeff-seq))

; ex 2.35
(define (acc-count-leaves t)
  (foldr (lambda (x y)
           (if (pair? x)
               (+ y (acc-count-leaves x))
               (+ y 1)))
         0
         t))

(define (acc-count-leaves2 t)
  (foldr +
         0
         (map (lambda (x)
                (if (pair? x)
                    (acc-count-leaves2 x)
                    1))
              t)))

; ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (foldr op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; ex 2.37
(define test-matrix '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define test-vector '(5 6 7 8))
(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (mat-row)
         (dot-product mat-row v))
       m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mrow)
           (matrix-*-vector cols mrow))
         m)))

; ex 2.38
(define (my-fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; ex 2.39
(define (reverse1 sequence)
  (foldr (lambda (x y)
           (append y (list x)))
         '()
         sequence))

(define (reverse2 sequence)
  (my-fold-left (lambda (x y)
                  (cons y x))
                '()
                sequence))

; ex 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; ex 2.41
(define (unique-triplets n)
  (flatmap (lambda (a)
             (flatmap (lambda (b)
                        (map (lambda (c) (list c b a))
                             (enumerate-interval 1 (- b 1))))
                      (enumerate-interval 1 (- a 1))))
           (enumerate-interval 1 n)))

(define (find-sum-triplets n sum)
  (filter (lambda (triplet)
            (= sum (foldr + 0 triplet)))
          (unique-triplets n)))

; ex 2.42
(define (same-diag f row-pos rest-positions)
  ;(printf "row-pos ~a ~a\n" row-pos rest-positions)
  (if (null? rest-positions)
      #f
      (or (= row-pos (car rest-positions))
          (same-diag f (f row-pos 1) (cdr rest-positions)))))
(define (safe? column positions)
  (define (same-row row-pos rest-positions)
    (ormap (lambda (x) (= x row-pos)) rest-positions))
  (let ([on-same-row (same-row (caar positions)
                               (map car (cdr positions)))]
        [on-same-diag-minus (same-diag - (- (caar positions) 1)
                                       (map car (cdr positions)))]
        [on-same-diag-plus (same-diag + (+ (caar positions) 1)
                                      (map car (cdr positions)))])
    ;(printf "same-row ~a, diag-minus ~a, diag-plus ~a, ~a, ~a\n"
    ;        on-same-row on-same-diag-minus on-same-diag-plus column positions)
    (and (not on-same-row) (not on-same-diag-minus) (not on-same-diag-plus))))
(define (adjoin-position row column positions)
  (cons (list row column) positions))
(define (queens board-size)
  (define empty-board '())
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; find all pythogareson triangles
(define (pythagoras-triangles x)
  (define (is-pythagoras nums)
    (let ((a (first nums))
          (b (second nums))
          (c (third nums)))
      (= (sqr c) (+ (sqr a) (sqr b)))))
  (filter is-pythagoras (unique-triplets x)))
