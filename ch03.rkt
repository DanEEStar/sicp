#lang racket

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

; ex 3.1

(define (make-accumulator sum)
  (lambda (value)
    (begin (set! sum (+ sum value))
           sum)))

; ex 3.2
(define (make-monitored f)
  (define callcount 0)
  (define (mf arg)
    (begin (set! callcount (+ callcount 1))
           (f arg)))
  (define (dispatch m)
    (if (eq? m 'how-many-calls)
        callcount
        (mf m)))
  dispatch
  )

; ex 3.3

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (if (eq? pw password)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request: MAKE-ACCOUNT"
                               m)))
            (error "Incorrect password" pw)))
  dispatch)
