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

(define (accumulate-n op initial sequences)
  (if (null? (car sequences))
      nil
      (cons (accumulate op initial (map car sequences))
            (accumulate-n op initial (map cdr sequences)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (col)
         (dot-product col v))
       m))

(define (transpose m)
  (accumulate-n cons (list) m))

(define m (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (col-of-m)
                (matrix-*-vector cols col-of-m))
                m)))

(matrix-*-matrix m (transpose m))




