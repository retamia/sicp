(define (square x)
  (* x x))

(define (fast-expt b n)
  (expt-iter b n 1))

(define (expt-iter b n a)
  (if (= n 0)
         a
       (if (even? n)
                (expt-iter (square b) (/ n 2) a)
                (expt-iter b (- n 1) (* a b))
                ))
