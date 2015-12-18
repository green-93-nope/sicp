(define (sqrt-stream1 x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (sqrt-stream2 x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream2 x))))

(define (sqrt-improve guess x)
  (display x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))
