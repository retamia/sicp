#lang planet neil/sicp

(define (square x)
  (* x x))

(define (map proc items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (proc (car things))
                    answer))))
  (iter (reverse items) nil))

(map square (list 1 2 3))