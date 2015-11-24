#lang planet neil/sicp

(define (for-each proc lst)
    (cond ((not (null? lst))
            (proc (car lst))
            (for-each proc (cdr lst)))))


(for-each (lambda (x) (newline) (display x))
          (list 57 38 23 21))

