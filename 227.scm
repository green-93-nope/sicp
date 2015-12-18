(define (deep-reverse x)
  (letrec ((deep-reverse-iter
            (lambda (result list)
              (cond ((null? list) result)
                    ((not (pair? (car list)))
                     (deep-reverse-iter
                      (cons (car list) result)
                      (cdr list)))
                    (else
                     (deep-reverse-iter
                      (cons (deep-reverse (car list))
                            result)
                      (cdr list)))))))
    (deep-reverse-iter '() x)))
