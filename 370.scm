(define (merge-weighted s1 s2 weight)
  (let ((first-s1 (stream-car s1))
        (first-s2 (stream-car s2))
        (weight-f1 (weight (stream-car s1)))
        (weight-f2 (weight (stream-car s2))))
    (cond ((< weight-f1 weight-f2)
           (cons-stream first-s1
                        (merge-weighted (stream-cdr s1)
                                        s2
                                        weight)))
          ((> weight-f1 weight-f2)
           (cons-stream first-s2
                        (merge-weighted s1
                                        (stream-cdr s2)
                                        weight)))
          (else
           (cons-stream first-s1
                        (cons-stream first-s2
                                     (merge-weighted (stream-cdr s1)
                                                     (stream-cdr s2)
                                                     weight)))))))

;; i <= j, and assume the first member of s1 and s2 is equal
(define (weighted-pairs s1 s2 weight)
  (cons-stream (list (stream-car s1) (stream-car s2))
               (merge-weighted (stream-map (lambda (x) (list (stream-car s1) x))
                                           (stream-cdr s2))
                               (weighted-pairs (stream-cdr s1) (stream-cdr s2) weight)
                               weight)))

(define (weight-a pair)
  (+ (car pair)
     (cadr pair)))

(define (pairs-a s1 s2)
  (weighted-pairs s1 s2 weight-a))

(define (weight-b pair)
  (let ((first (car pair))
        (second (cadr pair)))
    (+ (* 2 first) (* 3 second) (* 5 first second))))

(define (pairs-b s1 s2)
  (weighted-pairs s1 s2 weight-b))
