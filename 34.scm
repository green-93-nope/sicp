(define (make-account balance secret-password)
  (let ((count-incorrect-password 0))
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
          (begin
            (set! count-incorrect-password
                  (+ count-incorrect-password 1))
            (if (= count-incorrect-password 7)
                call-the-corps
                "Incorrect password")
            (cond ((eq? function 'withdraw)
                   (set! count-incorrect-password 0)
                   withdraw)
                  ((eq? function 'deposit)
                   (set! count-incorrect-password 0)
                   deposit)
              (else
               (error "Unknown request -- MAKE-ACCOUNT"
                      function)))))
  dispatch)
