#lang racket

(require rackunit "stream.rkt")
(require rackunit "integerate-series.rkt")

(define (integral integrand initial-value dt)
  (define int
    (cons-my-stream initial-value
                 (add-my-stream (scale-my-stream integrand dt)
                             int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-my-stream (scale-my-stream i R)
                   (integral (scale-my-stream i (/ 1 C)) v0 dt))))

(define RC1 (RC 5 1 0.5))

(define (sign-change-detector cur last)
  (cond [(and (> cur 0) (< last 0)) 1]
        [(and (< cur 0) (> last 0)) -1]
        [#t 0]))
(define (make-zero-crossings input-stream last-value)
  (cons-my-stream
   (sign-change-detector (my-stream-car input-stream) last-value)
   (make-zero-crossings (my-stream-cdr input-stream)
                        (my-stream-car input-stream))))
(define sense-data cosine-series)
(define zero-crossings (make-zero-crossings sense-data 0))
(define zero-crossings2
  (my-stream-map sign-change-detector sense-data (cons-my-stream 0 sense-data)))

(define (smooth s)
  (my-stream-map (lambda (x y) (/ (+ x y) 2))
                 s
                 (cons-my-stream 0 s)))
(define (make-zero-crossing2 input-stream)
  (let ([after-smooth (smooth input-stream)])
    (my-stream-map sign-change-detector
                   after-smooth
                   (cons-my-stream 0 after-smooth))))

  