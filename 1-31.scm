(define (rec-product term a next b)
  (if (> a b)
      1
      (* (term a)
         (rec-product term (next a) next b)
      )
  )
)

(define (iter-product term a next b)
  (define (iter a result)
   (if (> a b)
        result
        (iter (next a) (* (term a) result))
    ))     
 (iter a 1))

(define (factorial n)
  (iter-product (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(factorial 10)

(define (pi precision)
   (define (numer-product x)
     (cond ((= x 1) 2)
           ((even? x)(+ x 2))
           (else (+ x 1))))
   (define (denom-product x)
     (cond ((= x 1) 3)
           ((even? x) (+ x 1))
           (else (+ x 2))))
  (* 4
        (exact->inexact
            (/ (iter-product numer-product
                        1
                        (lambda (i) (+ i 1))
                        precision)
               (iter-product denom-product 
                        1
                        (lambda (i) (+ i 1))
                        precision))))
   )

(pi 100000)
