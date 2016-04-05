(load "constraint.scm")

(define (averager a b c)
  (let ((half (make-connector))
        (sum (make-connector)))
    (adder a b sum)
    (constant (/ 1 2) half)
    (multiplier sum half c)))

(define a (make-connector))

(define b (make-connector))

(define c (make-connector))

(probe "a" a)
(probe "b" b)
(probe "c" c)

(averager a b c)

(set-value! a 2 'user)

(set-value! b 4 'user)

(get-value c)
