;; a example to define a procedure of sqrt using sqrt-iter good-enough? square
(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter guess x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (improve guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2))
