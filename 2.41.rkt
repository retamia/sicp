#lang planet neil/sicp

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ 1 low) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc sequence)
  (accumulate append nil (map proc sequence)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)(list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (cons i j))
                  (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))

(define (triple-sum-equal-to? sum triple)
  (= sum
     (accumulate + 0 triple)))

(define (triples-equal-to sum triples)
  (filter (lambda (current-triple)
            (triple-sum-equal-to? sum current-triple))
          triples))

(triples-equal-to 10 (unique-triples 10))





