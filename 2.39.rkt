#lang planet neil/sicp

(define (reverse-old lst)
  (if (null? (cdr lst))
      lst
      (append (reverse (cdr lst))(list (car lst)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse-right sequence)
  (fold-right (lambda (x y)
                (append y (list x)))
              nil
              sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y)
                (append (list y) x))
              nil
              sequence))

(reverse-right (list 1 2 3))
(reverse-left (list 1 2 3))

