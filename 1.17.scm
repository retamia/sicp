(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (* a b)
  (if (= b 0)
      0
      (if (even? b)
          (double (* a (halve b)))
          (+ a (* a (- b 1)) ))))

(* 2 2)
