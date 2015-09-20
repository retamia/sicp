(define (double n)
  (* 2 n))

(define (halve n)
  (/ n 2))


;;递归计算过程
(define (mul-rec a b)
  (if (= b 0)
      0
      (if (even? b)
          (double (mul-rec a (halve b)))
          (+ a (mul-rec a (- b 1)) ))))

;;迭代计算过程
(define (mul-iter a b)
  (iter a b 0))

(define (iter a b n)
  (if (= b 0)
      n
      (if (even? b)
          (iter (double a) (halve b) n)
          (iter a (- b 1) (+ a n)))))

(mul-rec 2 100)



   


