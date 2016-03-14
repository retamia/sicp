#lang racket
(require compatibility/mlist)

(define nil (mlist))
(define exits-pairs (mlist))
(define (exits? pair x)
  (if (null? x)
      #f
      (or (eq? pair (mcar x))
          (exits? pair (mcdr x)))))

(define (count-pairs x)
  (if (not (mpair? x))
      0
      (if (exits? x exits-pairs)
          0
          (begin
            (set! exits-pairs (mcons x exits-pairs))
            (+ (count-pairs (mcar x))
               (count-pairs (mcdr x))
               1)))))

(define p1 (mcons 'a nil))
(define p2 (mcons p1 nil))
(define p3 (mcons p1 p2))
(count-pairs p3)
;Value: 3  在3.16题中返回4


