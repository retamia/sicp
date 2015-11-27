#lang planet neil/sicp

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree (list 1 2) 2)

(define (square x)
  (* x x))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree))( square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (map-square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (map-square-tree sub-tree)
             (square sub-tree)))
       tree))

(map-square-tree (list 1 2))
(square-tree (list 1 2))




