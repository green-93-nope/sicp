(define (fib n)
  (cond ((or (= n 1) (= n 0)) 1)
        (else
         (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib-result-equal? num)
  (define (fib-equal-iter num n)
    (let ((current-result (fib n)))
      (cond ((< num current-result) #f)
            ((= num current-result) n)
            (else
             (fib-equal-iter num (+ n 1))))))
  (fib-equal-iter num 1))
