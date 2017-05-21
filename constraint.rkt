#lang racket

(provide (all-defined-out))


(define (make-connector)
  (let ([value #f]
        [informant #f]
        [constraints '()])
    (define (set-my-value newval setter)
      (cond [(not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints)]
            [(not (= value newval))
             (error "Contradiciton" (list value newval))]
            [else 'ignored]))
    (define (forget-my-value retractor)
      (when (eq? informant retractor)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))))
    (define (connect new-constraint)
      (unless (memq new-constraint constraints)
             (set! constraints
                   (cons new-constraint constraints)))
      (when (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond [(eq? request 'has-value?)
             (if informant #t #f)]
            [(eq? request 'get-value) value]
            [(eq? request 'set-value!) set-my-value]
            [(eq? request 'forget) forget-my-value]
            [(eq? request 'connect) connect]
            [else
             (error "Unknown operation -- CONNECTOR" request)]))
    me))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'get-value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (for-each-except exception procedure list)
  (for-each (lambda (x) (procedure x))
            (filter (lambda (x) (eq? x exception)) list))
  'done)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond [(and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me)]
          [(and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me)]
          [(and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me)]))
  (define (process-forget-value)
    (forget-value! a1)
    (forget-value! a2)
    (forget-value! sum)
    (process-new-value))
  (define (me request)
    (cond [(eq? request 'I-have-a-value) (process-new-value)]
          [(eq? request 'I-lost-my-value) (process-forget-value)]
          [else
           (error "Unknown request -- ADDER" request)]))
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me))

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond [(eq? request 'I-have-a-value) (process-new-value)]
          [(eq? request 'I-lost-my-value) (process-forget-value)]
          [else
           (error "Unknown request -- PROBE" request)]))
  me)
