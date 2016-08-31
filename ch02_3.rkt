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

; ex 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(define testtree1 (list->tree '(1 2 3 4 5)))
(define testtree2 (list->tree '(1 3 5 7 9)))
(define testtree3 (list->tree '(2 4 6 8 10)))

; ex 2.65
(define (union-set-b set1 set2)
  (if (null? set1)
      set2
      (let ((s (adjoin-set-b (entry set1) set2)))
        (let ((left-result (union-set-b (left-branch set1) s)))
          (union-set-b (right-branch set1) left-result)))))

(define (intersection-set-b set1 set2)
  (define (inner set1 result)
    (if (null? set1)
        result
        (let ((r
               (if (element-of-set-b? (entry set1) set2)
                   (adjoin-set-b (entry set1) result)
                   result)))
          (let ((left-result (inner (left-branch set1) r)))
            (inner (right-branch set1) left-result)))))
  (inner set1 '()))

; 2.3.4 Huffman Encoding Trees
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch-h tree) (car tree))
(define (right-branch-h tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch-h branch))
        ((= bit 1) (right-branch-h branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set-h x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set-h x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set-h (make-leaf (car pair)
                                 (cadr pair))
                      (make-leaf-set (cdr pairs))))))

                                        ; ex 2.67
(define sample-tree-267
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-bits-267 '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define sample-message-267 (decode sample-bits-267 sample-tree-267))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (inner symbol current-branch)
    (if (leaf? current-branch)
        '()
        (let ((e? element-of-set-uo?)
              (left (left-branch-h current-branch))
              (right (right-branch-h current-branch)))
          (if (e? symbol (symbols left))
              (cons '0 (inner symbol left))
              (cons '1 (inner symbol right))))))
  (if (member symbol (symbols tree))
      (inner symbol tree)
      (error "bad symbol -- ENCODE-SYMBOL" symbol)))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (cond ((null? leaves) '())
        ((null? (cdr leaves)) (car leaves))
      (else (successive-merge (adjoin-set-h (make-code-tree (car leaves)
                                                      (cadr leaves))
                                      (cddr leaves))))))

(define sample-tree
  (generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1))))

; 2.70 rock song huffman
(define rocksong-tree
  (generate-huffman-tree '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9))))

(define rocksong-message '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(define rocksong-bits (encode rocksong-message rocksong-tree))

(define hello 'hello)
