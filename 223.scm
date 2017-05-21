(define (map procedure list)
  (if (null? list)
      '()
      (cons (procedure (car list))
            (map procedure (cdr list)))))

(define (for-each procedure list)
  (if (not (null? list))
      (if (null? (cdr list))
          (procedure (car list))
          (begin
            (procedure (car list))
            (for-each procedure (cdr list))))))
