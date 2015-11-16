#lang planet neil/sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (< d 0)
        (cons (- (/ n g)) (- (/ d g)))
        (cons (/ n g) (/ d g)))))

(define (numer rat)
  (car rat))

(define (demon rat)
  (cdr rat))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (demon y))
               (* (numer y) (demon x)))
            (* (demon x) (demon y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (demon y))
               (* (numer y) (demon x)))
            (* (demon x) (demon y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (demon x) (demon y))))

(define (div-rat x y)
  (make-rat (* (numer x) (demon y))
            (* (demon x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (demon y))
     (* (numer y) (demon x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (demon x)))

(define one-half (make-rat 2 4))

(print-rat one-half)