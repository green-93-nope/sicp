#lang racket

(require rackunit "problems.rkt")

(test-case
    "simple programs"

  )

(define mobile-test
  (test-suite
   "Test for mobile branch"
   (test-case
       "simple balance mobile"
     (let ([mobile (make-mobile (make-branch 2 4)
                                (make-branch 4 2))])
       (check-true (mobile-balance mobile) "simple mobile should be balance")
       (check-eq? (total-weight mobile) 6 "simple mobile should total weight 6")))
   (test-case
       "simple unbalance mobile"
     (let ([mobile (make-mobile (make-branch 2 7)
                                (make-branch 4 2))])
       (check-false (mobile-balance mobile) "simple mobile should be unbalance")
       (check-eq? (total-weight mobile) 9 "simple mobile should total weight 9")))
   (test-case
       "complex balance mobile"
     (let ([mobile (make-mobile (make-branch 2 (make-mobile (make-branch 3 2) (make-branch 2 (make-mobile (make-branch 1 2) (make-branch 2 1))))) (make-branch 5 2))])
       (check-true (mobile-balance mobile) "complex mobile should be balance")
       (check-eq? (total-weight mobile) 7 "complex mobile should total weight 11")))))

(require rackunit/text-ui)
(run-tests mobile-test)
