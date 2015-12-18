(define (make-monitored f)
  (let ((count 0))
    (lambda (mf)
      (cond ((eq? mf 'how-many-calls) count)
            ((eq? mf 'reset-count) (set! count 0))
            (else
             (begin
               (set! count (+ count 1))
               (f mf)))))))

(define (make-monitored f)
  (let ((count 0))
    (define (fun-and-count x)
      (begin
        (set! count (+ count 1))
        (f x)))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls) count)
            ((eq? m 'reset-count) (set! count 0))
            (else
             (fun-and-count m))))
    dispatch))
