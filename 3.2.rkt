#lang racket

(define (make-monitored f)
  (define calls 0)
  (define (dispatch op)
    (cond ((eq? op `how-many-calls?)
           calls)
          ((eq? op `reset-count)
           (set! calls 0))
          (else
           (begin (set! calls (+ calls 1))
             (f op)))))
  dispatch)

(define s (make-monitored sqrt))

(s 100)
(s `reset-count)
(s 5)
(s `how-many-calls?)
