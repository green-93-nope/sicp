#lang racket

(require rackunit "deriv.rkt")

(define deriv-test
  (test-suite
   "Test for deriv"
   (check-eq? (deriv 'a 'a) 1)
   (check-eq? (deriv 'a 'b) 0)
   (check-eq? (deriv 3 'a) 0)
   (test-case
       "make-sum deriv test"
     (let ([sum1 (make-sum (make-sum 'x 'y)
                           (make-sum 'x 2))])
       (check-eq? (deriv sum1 'x) '(sum (sum 1 . 0) sum 1 . 0))))
   (test-case
       "make-product deriv test"
     (let ([prod1 (make-product (make-sum 'x 'y)
                                (make-product 'x 2))])
       (check-eq? (deriv prod1 'x) '(product
              sum
              ((product x . 2) sum 1 . 0)
              (sum x . y)
              product
              sum
              (2 . 1)
              x
              .
              0))))
   ))

(require rackunit/text-ui)
(run-tests deriv-test)
