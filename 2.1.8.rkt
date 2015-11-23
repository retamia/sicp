#lang planet neil/sicp

(define (reverse lst)
  (if (null? (cdr lst))
      lst
      (append (reverse (cdr lst))(list (car lst)))))

(reverse (list 1 2 3))