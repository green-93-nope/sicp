(define (make-account balance secret-password)
  (define (withdraw amount)
    (if (< balance amount)
        "Insufficient funds"
        (begin
          (set! balance (- balance amount))
          balance)))
  (define (deposit amount)
    (begin
      (set! balance (+ balance amount))
      balance))
  (define (dispatch input-password function)
    (if (not (eq? input-password secret-password))
        "Incorrect password"
        (cond ((eq? function 'withdraw) withdraw)
              ((eq? function 'deposit) deposit)
              (else
               (error "Unknown request -- MAKE-ACCOUNT"
                      function)))))
  dispatch)
