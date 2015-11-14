#lang planet neil/sicp

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define (compose f g)
  (lambda (x)
    (f(g x))))

(define (repeated f times)
  (if (= times 1)
      f
      (compose f
               (repeated f (- times 1)))))

(define (smooth f)
  (let ((dx 0.0001))
    (lambda (x)
      (/ (+ (f (- x dx))
            (f x)
            (f (+ x dx)))
         3))))

(define (smooth-n-times f n)
  ((repeated smooth n) f))

((smooth-n-times square 5) 5)