#lang racket

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ 1 low) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc sequence)
  (accumulate append null (map proc sequence)))

(define empty-board (list))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (contains? item sequence)
  (if (null? sequence)
      #f
      (or (equal? item (car sequence))
          (contains? item (cdr sequence)))))

(define (no-repeat? positions)
  (if (null? positions)
      #t
      (and (not (contains? (car positions) (cdr positions)))
           (no-repeat? (cdr positions)))))

(define (safe? k positions)
  (define (px-y pl)
    (map + positions (enumerate-interval 1 k)))
  (define (px+y pl)
    (map - positions (enumerate-interval 1 k)))
  (and (no-repeat? positions)
       (no-repeat? (px-y positions))
       (no-repeat? (px+y positions))))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions)
                  (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)




