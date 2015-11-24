#lang planet neil/sicp

(define (fringe tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (list tree))
        (else (append (if (null? (car tree))
                          `()
                          (fringe (car tree)))
                      (fringe (cdr tree))))))

(define x (list (list 1 2)(list 3 4)))

(fringe (list x x))
