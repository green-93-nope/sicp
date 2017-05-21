(define (exist-ring? sets)
  (define (iter-check sets1 sets2)
    (cond ((or (null? sets1) (null? sets2) (null? (cdr sets2))) #t)
          ((eq? sets1 sets2)
           #f)
          (else
           (iter-check (cdr sets1) (cdr (cdr sets2))))))
  (iter-check sets (cdr sets)))
