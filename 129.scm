(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (next x) (+ x 1))
    (define (factor x)
      (cond ((or (= x 0) (= x n)) 1)
            ((odd? x) 4)
            (else
             2)))
    (define (term x)
      (* (factor x) (f (+ a (* x h)))))
    (* (/ h 3) (sum term 0 next n))))
