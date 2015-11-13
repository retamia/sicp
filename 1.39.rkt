#lang planet neil/sicp

(define (rec-cont-frac n d k)
  (define (f i)
    (if (= i k)
        (/ (n k) (d k))
        (/ (n i)
           (+ (d 1) (f (+ i 1))))))
  (f 1))

(define (iter-cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) result)))))
  (iter (- k 1)
        (/ (n k) (d k))))

(define (tan-cf x k)
  (iter-cont-frac (lambda (i)
                    (if (= i 1)
                        x
                        (- (* x x))))
                  (lambda (i)
                    (- (* i 2) 1))
                  k))

(tan-cf 10.0 100)