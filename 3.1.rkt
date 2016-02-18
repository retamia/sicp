#lang racket

(define (make-accumulator init)
  (lambda (number)
    (begin (set! init (+ init number))
           init)))

(define A (make-accumulator 5))
(A 10)

(A 10)
