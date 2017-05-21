#lang racket

(require rackunit
         "maketable.rkt")

(define test_table
  (test-suite
   "test for table"
   (test-case
       "simple test for table operation"
     (let ([table (make-table)])
       (check-false (lookup "a" table))
       (insert! "a" 2 table)
       (check-eq? (lookup "a" table) 2)
       (insert! "b" 3 table)
       (check-eq? (lookup "b" table) 3)
       (check-eq? (lookup "a" table) 2)
       (insert! "a" 4 table)
       (check-eq? (lookup "a" table) 4)))
   (test-case
       "simple test for double table operation"
     (let ([table (make-table2)])
       (check-false (lookup2 "a" "b" table))
       (insert2! "a" "b" 2 table)
       (check-eq? (lookup2 "a" "b" table) 2)
       (check-false (lookup2 "a" "c" table))
       (check-false (lookup2 "b" "a" table))
       (insert2! "a" "c" 3 table)
       (check-eq? (lookup2 "a" "b" table) 2)
       (check-eq? (lookup2 "a" "c" table) 3)
       (insert2! "b" "c" 4 table)
       (check-eq? (lookup2 "b" "c" table) 4)
       (insert2! "a" "b" 5 table)
       (check-eq? (lookup2 "a" "b" table) 5)))))

(require rackunit/text-ui)
(run-tests test_table)
