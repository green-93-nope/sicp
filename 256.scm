(load "ch232.scm")
(define (exponentiation? x)
  (and (pair? x) (eq? '** (car x))))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (make-exponentiation x y)
  (cond ((or (=number? y 0) (=number? x 1)) 1)
        ((=number? y 1) x)
        (else
         (list '** x y))))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var)
	     1
	     0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product
          (exponent exp)
          (make-product
           (make-exponentiation (base exp)
                                (- (exponent exp) 1))
           (deriv (base exp) var))))
	(else
	 (error "unknown expression type -- DERIV" exp))))

