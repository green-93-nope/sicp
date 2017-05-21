(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product
           (make-exponentiation (base exp)
                                (- (exponent exp) 1))
           (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (same-variable? x y)
  (and (variable? y) (variable? y) (eq? x y)))

(define (=number? x y)
  (and (number? x) (number? y) (= x y)))

(define (variable? x)
  (symbol? x))

(define (atom-in-list x)
  (and (pair? x) (null? (cdr x))))

(define (sum? exp)
  (and (pair? exp) (eq? '+ (car exp))))

(define (addend x)
  (cadr x))

(define (augend x)
  (caddr x))

(define (make-sum x y)
  (cond ((=number? x 0) y)
        ((=number? y 0) x)
        ((and (number? x) (number? y)) (+ x y))
        (else
         (list '+ x y))))

(define (product? exp)
  (and (pair? exp) (eq? '+ (car exp))))

(define (multiplier x)
  (cadr x))

(define (multiplicand x)
  (caddr x))

(define (make-product x y)
  (cond ((or (=number? x 0) (=number? y 0)) 0)
        ((=number? x 1) y)
        ((=number? y 1) x)
        ((and (number? x) (number? y)) (* x y))
        (else
         (list '* x y))))
