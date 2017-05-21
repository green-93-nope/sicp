(define (last-pair x)
  (cond ((null? x) '())
        ((null? (cdr x)) x)
        (else
         (last-pair (cdr x)))))
