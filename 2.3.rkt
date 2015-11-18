#lang planet neil/sicp

(define (square x)
  (* x x))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment))
                    (x-point (end-segment segment)))
                 2)
              (/ (+ (y-point (start-segment segment))
                    (y-point (end-segment segment)))
                 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define start-point (make-point 0 0))

(define end-point (make-point 0 2))

(define length (make-segment start-point end-point))

(define width (make-segment (make-point 0 0)
                            (make-point 2 0)))

(define (segment-length segment)
  (abs (sqrt (+ (square (- (x-point (start-segment segment))
                           (x-point (end-segment segment))))
                (square (- (y-point (start-segment segment))
                           (y-point (end-segment segment))))))))

;;第一种表现方式 根据长和宽 还有种根据4个线段，具体不展示了
;;
;;构造函数
(define (make-rectanger length width)
  (cons length width))

;;选择函数
(define (length-of-rectanger rectanger)
  (segment-length (car rectanger)))
;;选择函数
(define (width-of-rectanger rectanger)
  (segment-length (cdr rectanger)))




;;
(define (perimeter-rectanger rectanger)
  (let ((length (length-of-rectanger rectanger))
        (width (width-of-rectanger rectanger)))
    (* (+ length width)
       2)))

(define (area-rectanger rectanger)
  (let ((length (length-of-rectanger rectanger))
        (width (width-of-rectanger rectanger)))
    (* length width)))

(area-rectanger (make-rectanger length width))