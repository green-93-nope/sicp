#lang racket

(require rackunit
         "digitele.rkt")

(define test_queue
  (test-suite
   "tests for digit ele"
   (test-case
       "test for empty queue"
     (let ([queue (make-queue)])
       (check-true (empty-queue? queue))
       (check-exn exn:fail? (lambda () (front-queue queue)) "first front-queue should cause error")
       (check-exn exn:fail? (lambda () (delete-queue! queue)) "first delete-queue should cause error")
       ))
   (test-case
       "test for insert/delete queue"
     (let ([queue (make-queue)])
       (insert-queue! queue 1)
       (check-eq? (front-queue queue) 1 "front queue should be 1")
       (delete-queue! queue)
       (check-true (empty-queue? queue))
       (insert-queue! queue 2)
       (check-eq? (front-queue queue) 2)
       (insert-queue! queue 3)
       (insert-queue! queue 4)
       (delete-queue! queue)
       (delete-queue! queue)
       (check-eq? (front-queue queue) 4)))
   ))

(require rackunit/text-ui)

(run-tests test_queue)
