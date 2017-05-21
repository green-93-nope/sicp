(define (stream-limit S tolerance)
  (let ((next (stream-cdr S)))
    (if (< (abs (- (stream-car S) (stream-car next)))
           tolerance)
        (stream-car next)
        (stream-limit next tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream1 x) tolerance))
