#lang racket

(require rackunit "stream.rkt")
(provide (all-defined-out))

(define (integrate-series s)
  (div-my-stream s
                 integers))

(define exp-series
  (cons-my-stream 1 (integrate-series exp-series)))
(define cosine-series
  (cons-my-stream 1 (my-stream-map
                     (lambda (x) (* -1 x))
                     (integrate-series sine-series))))
(define sine-series
  (cons-my-stream 0 (integrate-series cosine-series)))
