(define (mul-stream s1 s2)
  (stream-map * s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-stream integers
                                            ones)))

(define factorials (cons-stream 1
                                (mul-stream factorials
                                            integers)))
