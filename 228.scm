(define (fringe x)
  (let ((leaf? (lambda (x)
                 (not (pair? x)))))
    (cond ((null? x) '())
          ((leaf? (car x))
           (append (list (car x)) (fringe (cdr x))))
          (else
           (append (fringe (car x)) (fringe (cdr x)))))))
