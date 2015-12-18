(define (reverse x)
  (letrec ((reverse-iter
            (lambda (result list)
              (cond ((null? list) result)
                    (else
                     (reverse-iter
                      (cons (car list)
                            result)
                      (cdr list)))))))
    (reverse-iter '() x)))
