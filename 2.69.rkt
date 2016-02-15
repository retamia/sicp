#lang racket

(define (make-leaf symbol weight)
  (list `leaf symbol weight))

(define (leaf? object)
  (eq? (car object) `leaf))

(define (symbol-leaf x)(cadr x))
(define (weight-leaf x)(caddr x))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (decode bits tree)
  (define (choose-branch bit tree)
    (cond ((= 0 bit) (left-branch tree))
          ((= 1 bit) (right-branch tree))
          (else (error "bad bit -- CHOOSE-BRANCH" bit))))
  (define (decode-1 bits current-branch)
    (if (null? bits)
        `()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (find exits? seq)
  (cond ((null? seq) false)
        (else (if (exits? (car seq))
                  true
                  (find exits? (cdr seq))))))

(define (encode-symbol symbol tree)
  (define (symbol-in-tree? symbol tree)
    (find (lambda (s) (eq? s symbol)) (symbols tree)))
  (cond ((leaf? tree) `())
        ((symbol-in-tree? symbol (left-branch tree))
         (cons 0
               (encode-symbol symbol (left-branch tree))))
        ((symbol-in-tree? symbol (right-branch tree))
         (cons 1
               (encode-symbol symbol (right-branch tree))))
        (else
         (error "This symbol not in tree: " symbol))))

(define (encode message tree)
  (if (null? message)
      `()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      `()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))
(define (successive-merge leaf-set)
  (cond ((= 0 (length leaf-set)) `())
        ((= 1 (length leaf-set))
         (car leaf-set))
        (else
         (let ((new-sub-tree (make-code-tree (car leaf-set) (cadr leaf-set)))
               (remained-leaf-set (cddr leaf-set)))
           (successive-merge (adjoin-set new-sub-tree remained-leaf-set))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))
