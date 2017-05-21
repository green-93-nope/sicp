#lang racket

(provide (all-defined-out))

;;; This part is for queue interferce
(define (make-queue) (mcons '() '()))
(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT-QUEUE with empty queue" queue)
      (mcar (front-ptr queue))))
(define (insert-queue! queue item)
  (let ([new-pair (mcons item null)])
    (if (empty-queue? queue)
        (begin
          (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair)
          queue)
        (begin
          (set-mcdr! (rear-ptr queue) new-pair)
          (set-rear-ptr! queue new-pair)
          queue))))
(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "DELETE-QUEUE with empty queue" queue)
      (set-front-ptr! queue (mcdr (front-ptr queue)))))


;;; This part is for digit wire
(define (make-wire)
  (let ([signal-value 0] [action-procedure '()])
    (define (set-my-signal! new-value)
      (if (not (= new-value signal-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedure))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedure (cons proc action-procedure))
      (proc))

    (define (dispatch m)
      (cond [(eq? m 'get-signal) signal-value]
            [(eq? m 'set-signal!) set-my-signal!]
            [(eq? m 'add-action!) accept-action-procedure!]
            [else (error "Unknown operation --WIRE" m)]))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ([first-item (first-agenda-item the-agenda)])
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define (make-time-segment time queue)
  (mcons time queue))
(define (segment-time s) (mcar s))
(define (segment-queue s) (mcdr s))

(define (make-agenda) (mcons 0 null))
(define the-agenda (make-agenda))

(define (current-time agenda) (mcar agenda))
(define (set-current-time! agenda time)
  (set-mcar! agenda time))
(define (segments agenda) (mcdr agenda))
(define (set-segments! agenda segments)
  (set-mcdr! agenda segments))
(define (first-segment agenda)
  (mcar (segments agenda)))
(define (rest-segments agenda)
  (mcdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belong-before? segments)
    (or (null? segments)
        (< time (segment-time (mcar segments)))))
  (define (make-new-time-segment time action)
    (let ([q (make-queue)])
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= time (segment-time (mcar segments)))
        (insert-queue! (segment-queue (mcar segments)) action)
        (let ([rest (mcdr segments)])
          (if (belong-before? rest)
              (set-mcdr! segments
                        (mcons (make-new-time-segment time action) rest))
              (add-to-segments! rest)))))
  (let ([segments (segments agenda)])
    (if (belong-before? segments)
        (set-segments! agenda
                       (mcons (make-new-time-segment time action) segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ([q (segment-queue (first-segment agenda))])
    (delete-queue! q)
    (when (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ([first-seg (first-segment agenda)])
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;;; This part is for gate circuit
(define (logical-not s)
  (cond [(= s 0) 1]
        [(= s 1) 0]
        [else
         (error "Invalid signal" s) ]))
(define (logical-and a b)
  (cond [(and (= a 1) (= b 1)) 1]
        [(or (= a 0) (= b 0)) 0]
        [else
         (error "Invalid signal" a b)]))
(define (logical-or a b)
  (cond [(or (= a 1) (= b 1)) 1]
        [(and (= a 0) (= b 0)) 0]
        [else
         (error "Invalid signal" a b)]))

(define inverter-delay 1)

(define (inverter input output)
  (define (inverter-input)
    (let ([new-value (logical-not (get-signal input))])
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input inverter-input)
  'ok)

(define and-gate-delay 2)
(define (and-gate a b output)
  (define (and-action-procedure)
    (let ([new-value (logical-and (get-signal a) (get-signal b))])
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a and-action-procedure)
  (add-action! b and-action-procedure)
  'ok)

(define or-gate-delay 2)
(define (or-gate a b output)
  (define (or-action-procedure)
    (let ([new-value (logical-or (get-signal a) (get-signal b))])
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a or-action-procedure)
  (add-action! b or-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let ([d (make-wire)]
        [e (make-wire)])
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)))

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define s (make-wire))
(probe "a" a)
(probe "b" b)
(probe "c" c)
(probe "s" s)
(half-adder a b s c)
