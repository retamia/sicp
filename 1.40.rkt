#lang planet neil/sicp

(define (average x y)
  (/ (+ x y)
     2
     ))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define tolerance 0.00001)

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x
       (/ (g x)
          ((deriv g) x)))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)) 
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define golden-section
  (fixed-point (average-damp (lambda (x) (+ 1 (/ 1 x))))
               1.0))

(define (sqrt x)
  (newton-method (lambda (y)
                   (- (square y) x))
                 1.0))

(define (cubic a b c)
  (lambda (x) (+
               (cube x)
               (* a (square x))
               (* b x) c)))

(newton-method (cubic 3 2 1) 1)