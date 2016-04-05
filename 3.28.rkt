#lang racket

(require compatibility/mlist)

(define (logical-or s1 s2)
  (cond ((or (= s1 1)(= s2 1)) 1)
        ((and (= s1 0)(= s2 0)) 1)
        (else (error "Invalid signal" s1 s2))))

(define (or-gate a1 a2 output)
  (define (or-action-proc)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-siganal! output new-value)))))
  (add-action! a1 or-action-proc)
  (add-action! a2 or-action-proc)
  'ok)

