#lang racket

(require compatibility/mlist)



(define (make-table same-key?)
  (let ((local-table (mlist `*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (mcar (mcar records))) (mcar records))
            (else (assoc key (mcdr records)))))
    (define (lookup key-1 key-2)
      (let ((sub-table (assoc key-1 (mcdr local-table))))
        (if sub-table
            (let ((record (assoc key-2 (mcdr sub-table))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((sub-table (assoc key-1 (mcdr local-table))))
        (if sub-table
            (let ((record (assoc key-2 (mcdr sub-table))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! sub-table (mcons (mcons key-2 value)
                                              (mcdr sub-table)))))
            (set-mcdr! local-table (mcons (mlist key-1
                                                 (mcons key-2 value))
                                          (mcdr local-table)))))
      `ok)
    (define (dispatch m)
      (cond ((eq? m `lookup) lookup)
            ((eq? m `insert!) insert!)
            (else
             (error "UNKNOW OPERATION" m))))
    dispatch))

(define number-table (make-table =))

((number-table `insert!) 1 1 2)

((number-table `lookup) 1 1)

