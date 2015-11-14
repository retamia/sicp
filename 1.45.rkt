#lang planet neil/sicp

(define (average x y)
  (/ (+ x y)
     2))

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define (lg n)
    (cond ((> (/ n 2) 1)
            (+ 1 (lg (/ n 2))))
          ((< (/ n 2) 1)
            0)
          (else
            1)))

(define (compose f g)
  (lambda (x)
    (f(g x))))

(define (repeated f times)
  (if (= times 1)
      f
      (compose f
               (repeated f (- times 1)))))

(define (expt base n)
  (if (= n 0)
      1
      ((repeated (lambda (x) (* base x)) n) 1)))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average-damp-times f n)
  ((repeated average-damp n) f))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.0001)) 
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (damped-nth-root n damp-times)
  (lambda (x)
    (fixed-point (average-damp-times
                  (lambda (y)
                   (/ x (expt y (- n 1))))
                  damp-times)
                 1.0)))

(define (nth-root n)
  (damped-nth-root n (lg n)))

((nth-root 3) 27)

