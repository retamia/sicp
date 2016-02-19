#lang racket

(define f
  (lambda (first)
    (set! f (lambda (second) first))
    first))

(+ (f 0) (f 1))

;; racket的求值顺序从左到右




