#lang racket

(require compatibility/mlist)

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

(define z (make-cycle (mlist `a `b `c)))

(define (has-cycle? x)
  (define (loop tortoise rabbit)
    (cond ((or (null? (mcdr tortoise))(not (mpair? tortoise))) #f)
          ((or (null? (mcdr rabbit))(not (mpair? rabbit))) #f)
          ((eq? tortoise rabbit) #t)
          (else
           (loop (mcdr tortoise) (mcdr (mcdr rabbit))))))
  (loop (mcdr x) (mcdr (mcdr x))))

(define y (mlist `a `b `c))
(has-cycle? z)
(has-cycle? y)
