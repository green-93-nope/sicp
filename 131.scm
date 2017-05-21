(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (factorial n)
  (product (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(define (square x)
  (* x x))

(define (pi n)
  (define (numer-term n)
    (cond ((= n 1) 2)
          ((odd? n) (+ n 1))
          (else
           (+ n 2))))
  (define (denom-term n)
    (cond ((odd? n) (+ n 2))
          (else
           (+ n 1))))
  (* 4 (exact->inexact
        (/ (product numer-term 1 (lambda (x) (+ x 1)) n)
          (product denom-term 1 (lambda (x) (+ x 1)) n)))))
