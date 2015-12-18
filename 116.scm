(define (fast-expt b n)
  (letrec ((fast-expt-iter (lambda (a b n)
                             (cond ((= n 0) a)
                                   ((even? n) (fast-expt-iter a (square b) (/ n 2)))
                                   (else
                                    (fast-expt-iter (* a b) b (- n 1)))))))
    (fast-expt-iter 1 b n)))

(define (square x)
  (* x x))
