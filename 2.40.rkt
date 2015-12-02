#lang planet neil/sicp

(define (filter predicate sequence)
  (cond ((null? sequence)nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (prime? n)
  (define (iter index)
    (cond ((= index n) #t)
          ((= (remainder n index) 0) #f)
          (else (iter (+ index 1)))))
  (iter 2))

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

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (make-prime-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(make-prime-pairs 6)



