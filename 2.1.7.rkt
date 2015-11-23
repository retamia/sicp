#lang planet neil/sicp

(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))

(last-pair (list))
