#lang racket

(require compatibility/mlist)
(require "queue.rkt")

(define (print-queue queue)
  (define (new-print front rear)
    (cond ((eq? front rear)(print (mcar rear))
                           (newline))
          (else
           (print (mcar front))
           (new-print (mcdr front) rear))))
  (if (empty-queue? queue)
      (mlist)
      (new-print (front-ptr queue) (rear-ptr queue))))

(define q1 (make-queue))

(print-queue (insert-queue! q1 `a))

(print-queue (insert-queue! q1 `b))

(print-queue (delete-queue! q1))
;由于racket的list的打印规则从头指针开始打印队列，一直遍历到队列的尾部逐一打印，
;然后尾指针开始继续打印导致尾指针所指向的pair被打印2次