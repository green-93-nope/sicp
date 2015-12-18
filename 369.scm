(define (triples S T U)
  (cons-stream
   (list (stream-car S) (stream-car T) (stream-car U))
   (interleave
    (stream-map (lambda (x)
                  (append (list (stream-car S)) x))
                (pairs (stream-cdr T) (stream-cdr U)))
    (triples (stream-cdr S) (stream-cdr T) (stream-cdr U)))))

(define (bida? x y z)
  (= (square z) (+ (square x) (square y))))

(define (square x)
  (* x x))

(define (bida-triples S T U)
  (stream-filter (lambda (l)
                   (apply bida? l))
                 (triples S T U)))
