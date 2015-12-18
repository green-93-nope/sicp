(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (define (proc i v0 v)
    (add-stream (integral (scale-stream i (/ 1 C))
                          v0
                          dt)
                (scale-stream i R)))
  proc)
