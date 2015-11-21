#lang planet neil/sicp

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


(define one
  (lambda (f)
    (lambda(x)
      (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define (+ first second)
  (lambda (f)
    (lambda (x)
      ((first f)((second f) x)))))

(define (f x)
  (display "0"))

((one f) 0)
(newline)
((two f) 0)
(newline)
(((+ one two)f) 0)
(newline)
(((+ (+ one two) one)f) 0)
