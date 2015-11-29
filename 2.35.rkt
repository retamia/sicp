#lang planet neil/sicp

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil
              sequence))


(define (count-leaves tree)
  (accumulate +
              0
              (map (lambda (sub-tree)
                     (if (not (pair? sub-tree))
                         1
                         (count-leaves sub-tree)))
                   tree)))

(count-leaves (list (list 1 2) (list 5 6) (list 3 4)))


