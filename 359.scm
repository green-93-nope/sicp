(define (div-stream s1 s2)
  (stream-map / s1 s2))

(define (integrate-series s)
  (div-stream s integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (stream-map (lambda (x) (* -1 x))
                             (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
