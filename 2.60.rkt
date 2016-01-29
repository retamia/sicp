#lang racket

(define (element-of-set? element set)
  (cond ((null? set) #f)
        ((equal? (car set) element) #t)
        (else (element-of-set? element (cdr set)))))

(define (adjoin-set element set)
  (cons set element))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) `())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (cons (car set1) (intersection-set (cdr set1) set2)))))

(define (union-set set1 set2)
  (if (null? set1)
      set2
     (cons (car set1) (union-set (cdr set1) set2))))

(intersection-set (list 1 2 2 4 54) (list 6 5 1 2 54))

;; adjon-set 集合效率O(1)
;; union-set 不用判断元素是否已经存在 效率为O(N)
;; 其他不变