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

(define (golden-ratio k)
  (+ 1
     (iter-cont-frac (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    k)))

(golden-ratio 11)