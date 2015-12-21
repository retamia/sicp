#lang racket
(require ( planet "sicp.ss" ( "soegaard" "sicp.plt" 2 1)))

(define (split big-combiner small-combiner)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split big-combiner small-combiner) painter (- n 1))))
          (big-combiner painter (small-combiner smaller smaller))))))

(define right-split (split beside below))

(define up-split (split below beside))

(paint (up-split einstein 8))
