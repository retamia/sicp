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

(define exits-pairs (mlist))
(define (exits? pair x)
  (if (null? x)
      #f
      (or (eq? pair (mcar x))
          (exits? pair (mcdr x)))))

(define (has-cycle? x)
  (if (mpair? x)
      (if (exits? x exits-pairs)
          #t
          (begin (set! exits-pairs (mcons x exits-pairs))
                 (or (has-cycle? (mcar x))
                     (has-cycle? (mcdr x)))))
      #f))

(define y (mlist `a `b `c))

(has-cycle? z)
(has-cycle? y)
;; 之前认为闭环是头尾写了个错误的。写完才想起来闭环不一定在头部就开始了可能是之后某一项
;;(define (has-cycle x)
;;  (define head (mcar x))
;;  (define (iter x)
;;    (cond ((null? x) #f)
;;          ((eq? (mcar x) head) #t)
;;          (else
;;          (iter (mcdr x)))))
;;  (iter (mcdr x)))
