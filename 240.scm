(load "239.scm")
(load "prime.scm")

(define (unique-pairs n)
  (flatmap (lambda (x)
             (map (lambda (y)
                    (list x y))
                  (enumerate-interval 1 (- x 1))))
           (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
