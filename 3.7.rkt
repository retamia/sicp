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

(define (make-joint acc origin-password new-password)
  (lambda (given-password mode)
    (if (eq? given-password new-password)
        (acc origin-password mode)
        (lambda (x)
          (display "Incorrect password")))))

(define peter-acc (make-account 100 `peter-password))

(define paul-acc (make-joint peter-acc `peter-password `paul-password))

((peter-acc 'peter-password 'withdraw) 50)
((paul-acc 'paul-password 'withdraw) 50)




