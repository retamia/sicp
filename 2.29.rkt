#lang planet neil/sicp

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))
  
(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (hang-other-mobile? branch)
  (pair? (branch-structure branch)))

(define (branch-weight branch)
  (if (hang-other-mobile? branch)
      (total-weight (branch-structure branch))
      (branch-structure branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (same-torque? branch other-branch)
  (= (branch-torque branch)
     (branch-torque other-branch)))

(define (mobile-balance? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (same-torque? left right)
         (branch-balance? left)
         (branch-balance? right))))

(define (branch-balance? branch)
  (if (hang-other-mobile? branch)
      (mobile-balance? (branch-structure))
      #t))

(define mobile (make-mobile (make-branch 1 50)(make-branch 1 50)))

(define other-mobile (make-mobile (make-branch 1 mobile) (make-branch 1 50)))

(mobile-balance? other-mobile)

(total-weight other-mobile)





