(define (exist-ring? sets)
  (define (iter-test sets exist-sets)
    (cond
     ((or (null? sets)
         (not (pair? sets))) #t)
     ((and (pair? sets)
           (memq sets exist-sets))
      #f)
     (else
      (iter-test (cdr sets) (cons sets exist-sets)))))
  (iter-test sets '()))
