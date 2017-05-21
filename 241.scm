(load "239.scm")

(define (three-pair n)
  (flatmap (lambda (x)
             (flatmap (lambda (y)
                    (map (lambda (z)
                           (list x y z))
                         (enumerate-interval 1 n)))
                  (enumerate-interval 1 n)))
           (enumerate-interval 1 n)))

(define (three-sum-equal-to pair s)
  (= (+ (car pair) (cadr pair) (caddr pair)) s))

(define (three-pair-not-equal pair)
  (let ((first (car pair))
        (second (cadr pair))
        (third (caddr pair)))
    (not (or (= first second)
             (= second third)
             (= first third)))))

(define (three-unequal-sums n s)
  (filter three-pair-not-equal
          (filter (lambda (x)
                    (three-sum-equal-to x s))
                    (three-pair n))))
