(define (div-series s1 s2)
  (cond ((= (stream-car s2) 0)
         (error "Error: The constant part of s2 is zero -- DIV-SERIES"))
        (else
         (let ((constant-coeff (stream-car s2)))
           (mul-series (scale-stream (find-x (scale-stream s2 (/ 1 constant-coeff)))
                                     constant-coeff)
                       s1)))))
