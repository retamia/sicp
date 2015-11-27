#lang planet neil/sicp

(define (map-tree f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (map-tree f sub-tree)
             (f sub-tree)))
       tree))

(define (square x)
  (* x x))

(define (square-tree tree)
  (map-tree square tree))

(square-tree (list 1 2))




