#lang racket

(define (make-account balance password)
  (define error-times 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ amount balance))
    balance)
  (define (call-the-cops)
    (display "call the cops"))
  (define (dispatch request-password m)
    (if (eq? request-password password)
        (cond ((eq? m `withdraw) withdraw)
          ((eq? m `deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT") m))
        (lambda (x) (if (>= error-times 6)
                        (call-the-cops)
                        (begin (set! error-times (+ error-times 1))
                      (display "Incorrect password"))))))
  dispatch)

(define acc (make-account 100 `secret-password))

((acc `secret-password  `withdraw) 40)

((acc `some-other-password  `deposit) 40)
((acc `some-other-password  `deposit) 40)
((acc `some-other-password  `deposit) 40)
((acc `some-other-password  `deposit) 40)
((acc `some-other-password  `deposit) 40)
((acc `some-other-password  `deposit) 40)
((acc `some-other-password  `deposit) 40)


