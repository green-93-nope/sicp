(define (find-x S)
  (cons-stream 1 (scale-stream
                  (mul-series (stream-cdr S)
                              (find-x S))
                               -1)))
