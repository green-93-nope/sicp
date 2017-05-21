(define (length x)
  (if (null? x)
      0
      (+ 1 (length x))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (+ (count-leaves (car x))
           (count-leaves (cdr x)))))
