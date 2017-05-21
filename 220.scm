(define (same-parity . parameters)
  (letrec ((item (car parameters))
           (judge (remainder item 2))
           (same-parity-iter
            (lambda (list)
              (cond ((null? list) (quote ()))
                    ((= (remainder (car list) 2) judge) (cons (car list)
                                                 (same-parity-iter (cdr list))))
                    (else
                     (same-parity-iter (cdr list)))))))
    (same-parity-iter parameters)))
