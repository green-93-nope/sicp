#! /usr/bin/env racket
#!r6rs

(import (rnrs)
        (rnrs lists (6))
        (rnrs base (6))
        (rnrs mutable-pairs (6))
        (rnrs hashtables (6))
        (rnrs io simple (6)))

(define apply-in-underlying-scheme apply)

(define ht (make-eq-hashtable))
(define (put op type item)
  (if (hashtable-contains? ht op)
      (let ([optable (hashtable-ref ht op #f)])
        (hashtable-set! optable type item))
      (let ([optable (make-eq-hashtable)])
        (hashtable-set! optable type item)
        (hashtable-set! ht op optable))))

(define (get op type)
  (hashtable-ref (hashtable-ref ht op #f) type #f))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (apply-meta op exp env)
  (let ([type-tag (type-tag exp)])
    (let ([proc (get op type-tag)])
      (if proc
          (proc exp env)
          (if (application? exp)
              (meta-apply (meta-eval (operator exp) env)
                          (list-of-values (operands exp) env))
              (error
               "No method for these types --- APPLY-GENERIC"
               (list op type-tag)))))))

(define (meta-eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [#t (apply-meta 'meta-eval exp env)]
        ))

(define (self-evaluating? exp)
  (cond [(number? exp) #t]
        [(string? exp) #t]
        [#t #f]))

(define (variable? exp)
  (symbol? exp))

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

(define (install-quote-package)
  (define (text-of-quotation exp)
    (cadr exp))
  (define (eval-quote exp env)
    (text-of-quotation exp))

  ;; interface to the rest of system
  (put 'meta-eval 'quote eval-quote)
  )
(install-quote-package)

(define (install-assignment-package)
  (define (assignment-variable exp)
    (cadr exp))
  (define (assignment-value exp)
    (caddr exp))
  (define (make-assignment var exp)
    (list 'set! var exp))
  (define (set-variable-value! var val env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond [(null? vars)
               (env-loop (enclosing-environment env))]
              [(eq? var (car vars))
               (set-car! vals val)]
              [#t
               (scan (cdr vars) (cdr vals))]))
      (if (eq? env the-empty-environment)
          (error "unbound variable -- SET!" var)
          (let ([frame (first-frame env)])
            (scan (frame-variables frame)
                  (frame-values frame)))))
    (env-loop env))
  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (meta-eval (assignment-value exp) env)
                         env)
    'ok)

  ;; interface to the rest of system
  (define (tag x) (attach-tag 'set! x))
  (put 'meta-eval 'set! eval-assignment)
  (put 'make-assignment 'set! make-assignment)
  )
(install-assignment-package)

(define (make-assignment var exp)
  ((get 'make-assignment 'set!) var exp))

(define (install-define-package)
  (define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
  (define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)
                     (cddr exp))))

  (define (define-variable! var val env)
    (let ([frame (first-frame env)])
      (define (scan vars vals)
        (cond [(null? vars)
               (add-binding-to-frame! var val frame)]
              [(eq? var (car vars))
               (set-car! vals val)]
              [#t (scan (cdr vars) (cdr vals))]))
      (scan (frame-variables frame)
            (frame-values frame))))
  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
      (meta-eval (definition-value exp) env)
      env)
    'ok)

  ;; interface to the rest of system
  (define (tag x) (attach-tag 'define x))
  (put 'meta-eval 'define eval-definition)
  (put 'define-variable! 'define define-variable!)
  )
(install-define-package)

(define (define-variable! var val env)
  ((get 'define-variable! 'define) var val env))

(define (install-lambda-package)
  (define (lambda-parameter exp)
    (cadr exp))
  (define (lambda-body exp)
    (cddr exp))
  (define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))
  (define (eval-lambda exp env)
    (make-procedure (lambda-parameter exp)
                    (lambda-body exp)
                    env))
    ;; interface to the rest of the system
  (define (tag x) (attach-tag 'lambda x))
  (put 'meta-eval 'lambda eval-lambda)
  (put 'make-lambda 'lambda make-lambda)
  )
(install-lambda-package)

(define (make-lambda parameters body)
  ((get 'make-lambda 'lambda) parameters body))

(define (install-let-package)
  (define (let-body exp)
    (cddr exp))
  (define (let-parameter exp)
    (cadr exp))
  (define (let->combination args body)
    (let ([vars (map car args)]
          [exps (map cadr args)])
      (cons (make-lambda vars body) exps)))
  (define (eval-let exp env)
    (meta-eval
     (let->combination (let-parameter exp)
                       (let-body exp))
     env))
  (define (make-let parameter body)
    (cons 'let (cons parameter body)))

  ;; interface to the rest of system
  (put 'meta-eval 'let eval-let)
  (put 'make-let 'let make-let)
  )
(install-let-package)

(define (make-let parameter body)
  ((get 'make-let 'let) parameter body))

(define (install-let*-package)
  (define (let*-body exp)
    (cddr exp))
  (define (let*-parameter exp)
    (cadr exp))
   (define (let*->nested-lets exp)
    (define (let*->lets para body)
      (if (or (null? para) (null? (cdr para)))
          (make-let para body)
          (let ([rest (let*->lets (cdr para) body)])
            (make-let (list (car para))
                      (list rest)))))
    (let*->lets (let*-parameter exp)
                (let*-body exp)))
  (define (eval-let* exp env)
    (meta-eval (let*->nested-lets exp) env))

  ;; interface to the rest of system
  (put 'meta-eval 'let* eval-let*)
  )
(install-let*-package)

(define (install-letrec-package)
  (define (letrec-body exp)
    (caddr exp))
  (define (letrec-parameter exp)
    (cadr exp))
  (define (letrec->let exp)
    (let ([para (letrec-parameter exp)]
          [body (letrec-body exp)])
      (let ([vars (map car para)]
            [vals (map cadr para)])
        (make-let (map (lambda (x) (list x ''*unassigned*)) vars)
                  (append (map (lambda (x y) (make-assignment x y)) vars vals)
                          (list body))))))
  (define (eval-letrec exp env)
    (meta-eval (letrec->let exp) env))

  ;; interface to the rest of the system
  (put 'meta-eval 'letrec eval-letrec)
  )
(install-letrec-package)

(define (install-if-package)
  (define (if-predicate exp)
    (cadr exp))
  (define (if-consequent exp)
    (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        #f))
  (define (eval-if exp env)
    (if (true? (meta-eval (if-predicate exp) env))
        (meta-eval (if-consequent exp) env)
        (meta-eval (if-alternative exp) env)))
  (define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'if x))
  (put 'meta-eval 'if eval-if)
  (put 'make-if 'if make-if)
  )
(install-if-package)

(define (make-if predicate consequent alternative)
  ((get 'make-if 'if) predicate consequent alternative))

(define (install-begin-package)
  (define (begin-actions exp)
    (cdr exp))
  (define (last-exp? seq)
    (null? (cdr seq)))
  (define (first-exp seq)
    (car seq))
  (define (rest-exps seq)
    (cdr seq))
  (define (eval-sequence exps env)
    (cond [(last-exp? exps) (meta-eval (first-exp exps) env)]
          [#t (meta-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env)]))
  (define (eval-begin exps env)
    (eval-sequence (begin-actions exp) env))
  (define (sequence->exp seq)
    (cond [(null? seq) seq]
          [(last-exp? seq) (first-exp seq)]
          [#t (make-begin seq)]))
  (define (make-begin seq)
    (cons 'begin seq))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'begin x))
  (put 'meta-eval 'begin eval-begin)
  (put 'make-begin 'begin make-begin)
  (put 'sequence->exp 'begin sequence->exp)
  (put 'eval-sequence 'begin eval-sequence)
  )
(install-begin-package)
(define (make-begin seq)
  ((get 'make-begin 'begin) seq))
(define (sequence->exp seq)
  ((get 'sequence->exp 'begin) seq))
(define (eval-sequence exp env)
  ((get 'eval-sequence 'begin) exp env))

(define (install-cond-package)
  (define (cond-clauses exp)
    (cdr exp))
  (define (cond-else-clauses? clause)
    (eq? (cond-predicate clause) 'else))
  (define (cond-predicate clause)
    (car clause))
  (define (cond-actions clause)
    (cdr clause))
  (define (cond->if exp)
    (expand-clauses (cond-clauses exp)))
  (define (expand-clauses clauses)
    (if (null? clauses)
        #f
        (let ([first (car clauses)]
              [rest (cdr clauses)])
          (if (cond-else-clauses? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "ELSE clauses isn't last -- COND->IF" clauses))
              (make-if (cond-predicate first)
                       (sequence->exp (cond-actions first))
                       (expand-clauses rest))))))
  (define (eval-cond exp env)
    (meta-eval (cond->if exp) env))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'cond x))
  (put 'meta-eval 'cond eval-cond)
  )
(install-cond-package)

(define (true? x)
  (not (eq? x #f)))
(define (false? x)
  (eq? x #t))

(define (application? exp)
  (pair? exp))
(define (operator exp)
  (car exp))
(define (operands exp)
  (cdr exp))
(define (no-operands? ops)
  (null? ops))
(define (first-operands ops)
  (car ops))
(define (rest-operands ops)
  (cdr ops))

(define (meta-apply procedure arguments)
  (cond [(primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments)]
        [(compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure)))]
        [#t
         (error "Unknown procedure type -- APPLY" procedure)]))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (meta-eval (first-operands exps) env)
            (list-of-values (rest-operands exps) env))))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame)
  (car frame))
(define (frame-values frame)
  (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (error "Wrong match arguments supplied" vars vals)))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "Access unassigned variable" var)
                 (car vals))]
            [#t
             (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc)
  (cadr proc))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'list list)
        (list 'null? null?)
        (list '* *)
        (list '+ +)
        (list '- -)
        (list '/ /)
        
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ([initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)])
    (define-variable! 'true true? initial-env)
    (define-variable! 'false false? initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define input-prompt ":::M-Eval input:")
(define output-prompt ":::M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ([input (read)])
    (let ([output (meta-eval input the-global-environment)])
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(driver-loop)


