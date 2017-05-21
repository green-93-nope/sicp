(define (subset s)
  (cond ((null? s) (list '()))
        (else
         (let ((rest (subset (cdr s))))
         (append (map (lambda (x) (cons (car s) x)) rest))
                 rest))))
