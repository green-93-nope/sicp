(define (count-pairs sets)
  (length (no-same-in-list sets '())))

(define (no-same-in-list sets memo-sets)
  (cond ((or (not (pair? sets)) (memq sets memo-sets))
         memo-sets)
        (else
         (no-same-in-list (car sets)
                          (no-same-in-list (cdr sets)
                                           (cons sets memo-sets))))))
