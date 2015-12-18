(define (make-member item)
  (cons item (cons '() '())))
(define (value-member member)
  (car member))
(define (ptr-member member)
  (cdr member))
(define (former-member member)
  (cadr member))
(define (next-member member)
  (cdr (cdr member)))
(define (set-next-member! member item)
  (set-cdr! (ptr-member member) item))
(define (set-former-member! member item)
  (set-car! (ptr-member member) item))

(define (front-ptr deque)
  (car deque))
(define (rear-ptr deque)
  (cdr deque))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (make-deque)
  (cons '() '()))
(define (empty-deque? deque)
  (or (null? (front-ptr deque))
      (null? (rear-ptr deque))))
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (rear-ptr deque))))

(define (front-print-deque deque)
  (define (iter-print-member member)
    (cond ((null? (next-member member))
           (display (value-member member))
           (newline))
          (else
           (display (value-member member))
           (display " ")
           (iter-print-member (next-member member)))))
  (iter-print-member (front-ptr deque)))

(define (front-insert-deque! deque item)
  (let ((new-member (make-member item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-member)
           (set-rear-ptr! deque new-member)
           deque)
          (else
           (set-next-member! new-member (front-ptr deque))
           (set-former-member! (front-ptr deque) new-member)
           (set-front-ptr! deque new-member)
           deque))))

(define (rear-insert-deque! deque item)
  (let ((new-member (make-member item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-member)
           (set-rear-ptr! deque new-member)
           deque)
          (else
           (set-former-member! new-member (rear-ptr deque))
           (set-next-member! (rear-ptr deque) new-member)
           (set-rear-ptr! deque new-member)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-front-ptr! deque (next-member (front-ptr deque)))
         (if (empty-deque? deque)
             (set-rear-ptr! deque '())
         (set-former-member! (front-ptr deque) '()))
         deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-rear-ptr! deque (former-member (rear-ptr deque)))
         (if (empty-deque? deque)
             (set-front-ptr! deque '())
          (set-next-member! (rear-ptr deque) '()))
         deque)))
