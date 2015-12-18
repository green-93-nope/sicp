(define memo-fib
  (memorize (lambda (n)
              (cond ((= n 0) 0)
                    ((= n 1) 1)
                    (else
                     (+ (memo-fib (- n 1))
                        (memo-fib (- n 2))))))))

(define (memorize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previous-computed-result (lookup x table)))
        (or previous-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))
