#lang racket

(require "binary-tree.rkt")

(define (key entry-tree)
  (car entry-tree))

(define (lookup given-key tree-of-records)
    (if (null? tree-of-records)                                             
        #f
        (let ((entry-key (key (entry tree-of-records))))                    
            (cond ((= given-key entry-key)                                  
                    (entry tree-of-records))                                
                  ((> given-key entry-key)
                    (lookup given-key (right-branch tree-of-records)))
                  ((< given-key entry-key)
                    (lookup given-key (left-branch tree-of-records)))))))

(lookup 1 (make-tree `(1 "王讨厌") `() `()))