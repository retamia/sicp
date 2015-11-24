#lang planet neil/sicp

(define (square x)
  (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map (lambda (x) (* x x)) items))

(square-list-map (list 1 2 3))