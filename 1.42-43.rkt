#lang planet neil/sicp

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define (compose f g)
  (lambda (x)
    (f(g x))))

((compose square inc) 6)

(define (repeated f times)
  (if (= times 1)
      f
      (compose f
               (repeated f (- times 1)))))

((repeated square 2) 5)