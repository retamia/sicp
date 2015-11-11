(define (rec-accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (rec-accumulate combiner null-value term (next a) next b)
      )
  ))

(define (iter-accumulate combiner null-value term a next b)
  (define (iter combiner a result)
    (if (> a b)
        result
        (iter combiner (next a) (combiner (term a) result))))
  (iter combiner a null-value))


(define (rec-product term a next b)
  (rec-accumulate *
                1 
                term
                a
                next
                b))

(define (iter-product term a next b)
  (iter-accumulate *
                1 
                term
                a
                next
                b))

(define (factorial n)
  (iter-product (lambda (x) x) 1 (lambda (x)(+ x 1)) n))

(factorial 10)

