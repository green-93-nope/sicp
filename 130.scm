(define (sum term a next b)
  (define (sum-iter a result)
    (if (> a b)
        result
        (sum-iter (next a) (+ result (term a)))))
  (sum-iter a 0))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (square x)
  (* x x))
(define (identity x)
  x)

(define (inc x)
  (+ x 1))

(define (sum-identity a b)
  (sum identity a inc b))
