#lang planet neil/sicp
(define (inc i)
  (+ i 1))

(define (double f-one-param)
  (lambda (x) (f-one-param (f-one-param x))))

(((double (double double)) inc) 5)