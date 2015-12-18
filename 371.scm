(define (weight-sr pair)
  (+ (cube (car pair))
     (cube (cadr pair))))

(define (cube x)
  (* x x x))

(define (equal-stream-next s)
  (let ((next (stream-car (stream-cdr s))))
    (= (stream-car s) next)))

(define (pairs-cube s1 s2)
  (weighted-pairs s1 s2 weight-sr))

(define stream-pairs-cube
  (pairs-cube integers integers))

(define pairs-sr
  (let ((equal-num 0))
    (define (equal-stream-formal pair)
      (let ((val (weight-sr pair)))
        (if (= val equal-num)
            (begin (set! equal-num 0)
                   #t)
            (begin (set! equal-num val)
                   #f))))
    (stream-filter equal-stream-formal
                   stream-pairs-cube)))
