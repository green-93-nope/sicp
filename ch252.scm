(define put-coercion put)
(define get-coercion get)

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(define (schene-number->rational n)
  (make-rational (contents n) 1))

(define (rational->complex n)
  (make-complex-from-real-imag (div (numer (contents x)) (denom (contents x)))
                               0))
(put-coercion 'scheme 'complex scheme-number->complex)
