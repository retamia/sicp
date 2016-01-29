#lang racket

(define (element-of-set? element set)
  (cond ((null? set) #f)
        ((equal? (car set) element) #t)
        (else (element-of-set? element (cdr set)))))

(define (adjoin-set element set)
  (if (element-of-set? element set)
      set
      (cons set element)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) `())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (if (element-of-set? (car set1) set2)
          (union-set (cdr set1) set2)
          (cons (car set1) (union-set (cdr set1) set2)))))

(union-set (list 1 2 4 54) (list 6 5 1 2 54))