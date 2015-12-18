(load "236.scm")
(load "238.scm")

(define (reverse seq)
  (accumulate (lambda (x y)
                (append y (list x)))
              '()
              seq))

(define (reverse-left seq)
  (fold-left (lambda (x y)
               (append x (list y)))
             '()
             seq))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x)
            (not (= x item)))
          sequence))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else
         (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval (+ low 1) high))))
