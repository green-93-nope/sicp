(define (rand-update x)
  (remainder (+ (* x 13) 5) 24))

(define random-init (expt 2 32))
(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate)
             (begin
               (set! x (rand-update x))
               x))
            ((eq? m 'reset)
             (lambda (new-value)
               (set! x new-value)))))))
