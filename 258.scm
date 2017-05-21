(load "ch232.scm")

(define (make-sum x y)
  (cond ((=number? x 0) y)
        ((=number? y 0) x)
        ((and (number? x) (number? y)) (+ x y))
        (else
         (list x '+ y))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend x)
  (car x))

(define (augend x)
  (caddr x))

(define (make-product x y)
  (cond ((or (=number? x 0) (=number? y 0)) 0)
        ((=number? x 1) y)
        ((=number? y 1) x)
        ((and (number? x) (number? y)) (* x y))
        (else
         (list x '* y))))

(define (multiplier x)
  (car x))

(define (multiplicand x)
  (caddr x))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
