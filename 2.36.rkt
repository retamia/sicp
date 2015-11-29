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

(define (accumulate-n op initial sequences)
  (if (null? (car sequences))
      nil
      (cons (accumulate op initial (map car sequences))
            (accumulate-n op initial (map cdr sequences)))))

(accumulate-n + 0 (list (list 1 2 3)(list 4 5 6)(list 7 8 9)(list 10 11 12)))


