#lang planet neil/sicp

(define (reverse lst)
  (if (null? (cdr lst))
      lst
      (append (reverse (cdr lst))(list (car lst)))))

(define (deep-reverse tree)
  (cond ((null? tree) `())
        ((not (pair? tree)) tree)
        (else
         (append (deep-reverse (cdr tree))
                        (list (deep-reverse (car tree)))))))

(define x (list (list 1 2)(list 3 4)))

x

(reverse x)

(deep-reverse x)

