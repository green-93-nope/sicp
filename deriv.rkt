#lang racket

(provide (all-defined-out))

(define ht (make-hash))
(define (put op type item)
  (hash-set! ht (cons op type) item))
(define (get op type)
  (hash-ref ht (cons op type)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (apply proc
                 (map contents args))
          (error
           "No method for these types --- APPLY-GENERIC"
           (list op type-tags))))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (variable? exp) (symbol? exp))
(define (same-variable? exp var)
  (and (variable? exp)
       (variable? var)
       (eq? exp var)))
(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [#t ((get 'deriv (operator exp))
             (operands exp)
             var)]))

(define (install-sum-package)
  (define (added exp) (car exp))
  (define (augend exp) (cdr exp))
  (define (make-sum a b) (cons a b))
  (define (deriv-sum exp var)
    (make-sum (deriv (added exp) var)
              (deriv (augend exp) var)))
  ;; interface
  (define (tag x) (attach-tag 'sum x))
  (put 'deriv 'sum
       (lambda (exp var) (tag (deriv-sum exp var))))
  (put 'make-sum 'sum
       (lambda (x y) (tag (make-sum x y))))
  'done
  )
(install-sum-package)
(define (make-sum x y)
  ((get 'make-sum 'sum) x y))

(define (install-product-package)
  (define (multiplicand exp) (car exp))
  (define (multiplier exp) (cdr exp))
  (define (make-product a b) (cons a b))
  (define (deriv-product exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (multiplicand exp)
                   (deriv (multiplier exp) var))))
  (define (tag x) (attach-tag 'product x))
  (put 'deriv 'product
       (lambda (exp var) (tag (deriv-product exp var))))
  (put 'make-product 'product
       (lambda (x y) (tag (make-product x y))))
  'done)
(install-product-package)
(define (make-product x y)
  ((get 'make-product 'product) x y))
