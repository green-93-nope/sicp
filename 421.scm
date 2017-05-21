(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)                   ;function of even
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)                   ;function of odd
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(define (fib n)
  ((lambda (fib-iter)
     (fib-iter fib-iter n))
   (lambda (fib-iter k)
     (if (or (= k 0) (= k 1))
         1
         (+ (fib-iter fib-iter (- k 1)) (fib-iter fib-iter (- k 2)))))))
