#lang racket

(require compatibility/mlist)
(require "queue.rkt")

(define q1 (make-queue))

(insert-queue! q1 `a)

(insert-queue! q1 `b)

;(delete-queue! q1)

;(delete-queue! q1)

(define (print-queue queue)
  (define (new-print front rear)
    (cond ((eq? front rear)(print (mcar rear)))
          (else
           (print (mcar front))
           (new-print (mcdr front) rear))))
  (if (empty-queue? queue)
      (mlist)
      (new-print (front-ptr queue) (rear-ptr queue))))

(print-queue q1)
