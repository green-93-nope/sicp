;;;;LAZY EVALUATOR FROM SECTION 4.2 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm
;;;; Also includes enlarged primitive-procedures list

;;;;This file can be loaded into Scheme as a whole.
;;;;**NOTE**This file loads the metacircular evaluator of
;;;;  sections 4.1.1-4.1.4, since it uses the expression representation,
;;;;  environment representation, etc.
;;;;  You may need to change the (load ...) expression to work in your
;;;;  version of Scheme.
;;;;**WARNING: Don't load mceval twice (or you'll lose the primitives
;;;;  interface, due to renamings of apply).

;;;;Then you can initialize and start the evaluator by evaluating
;;;; the two lines at the end of the file ch4-mceval.scm
;;;; (setting up the global environment and starting the driver loop).


;;;;  To run without memoization, reload the first version of force-it below


;;**implementation-dependent loading of evaluator file
;;Note: It is loaded first so that the section 4.2 definition
;; of eval overrides the definition from 4.1.1


;;;SECTION 4.2.2

;;; Modifying the evaluator
(define (make-procedure-define var parameters body)
   (list 'define (cons var parameters) body))
(define (let->combination exp)
  (if (pair? (cadr exp))
      (let ((parameters (let-parameters exp)))
        (cons
         (make-lambda (let-variables parameters)
                      (let-body exp))
         (let-exps parameters)))
      (let ((arguments (cdr exp)))
        (let ((var (car arguments))
              (parameters (map car (cadr arguments)))
              (values (map cadr (cadr arguments))))
          (make-begin (list
                       (make-procedure-define var
                                   parameters
                                   (caddr arguments))
                      (cons var values)))))))

(define apply-in-underlying-scheme apply)

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin exp) (cons 'begin exp))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (let ((actions (cond-actions first)))
            (if (and (not (null? actions)) (eq? (car actions) '=>))
                (let ((predicate-result (list (cadr actions) (cond-predicate first))))
                  (make-if (cond-predicate first)
                           predicate-result
                           (expand-clauses rest)))
                (make-if (cond-predicate first)
                         (sequence->exp actions)
                         (expand-clauses rest))))))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-parameters exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-variables parameters)
  (if (null? parameters)
      '()
      (cons (caar parameters)
            (let-variables (cdr parameters)))))
(define (let-exps parameters)
  (if (null? parameters)
      '()
      (cons (cadar parameters)
            (let-exps (cdr parameters)))))

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-parameters exp) (cadr exp))
(define (first-let*-parameter parameters) (car parameters))
(define (rest-let*-parameters parameters) (cdr parameters))
(define (let*-body exp) (caddr exp))
(define (make-let parameters body)
  (cons 'let
        (cons parameters body)))
(define (expand-let*-clause parameters body)
  (if (null? parameters)
      (make-let '() body)
      (make-let (list (first-let*-parameter parameters))
                (expand-let*-clause (rest-let*-parameters parameters) body))))
(define (let*->nested-lets exp)
  (expand-let*-clause (let*-parameters exp) (let*-body exp)))

(define true #t)
(define false #f)
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))
;; new adder
(define (and? exp) (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))
(define (first-and-part exp) (car exp))
(define (rest-and-part exp) (cdr exp))
(define (eval-and exp env)
  (define (evaln-and exp env)
    (if (null? exp)
        'true
        (let ((first-result (eval (first-and-part exp) env)))
          (if (true? first-result)
              (evaln-and (rest-and-part exp) env)
              'false))))
  (evaln-and (and-clauses exp) env))

(define (or? exp) (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))
(define (first-or-part exp) (car exp))
(define (rest-or-part exp) (cdr exp))
(define (eval-or exp env)
  (define (evaln-or exp env)
    (if (null? exp)
        'false
        (let ((first-result (eval (first-or-part exp) env)))
          (if (true? first-result)
              'true
              (evaln-or (rest-or-part exp) env)))))
  (evaln-or (or-clauses exp) env))

(define (and->if exp) (expand-and-clauses (and-clauses exp)))
(define (expand-and-clauses clauses)
  (if (null? clauses)
      'true
      (make-if (first-and-part clauses)
               (expand-and-clauses (rest-and-part clauses))
               'false)))
(define (or-if exp) (expand-or-clauses (or-clauses exp)))
(define (expand-or-clauses clauses)
  (if (null? exp)
      'false
      (make-if (first-and-part clauses)
            'true
            (expand-or-clauses (rest-or-part clauses)))))

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
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable --SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))



(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cddr cddr)
        (list 'cons cons)
        (list 'list list)
        (list 'null? null?)
        (list 'pair? pair?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '> >)
        (list '< <)
        (list 'display display)
        (list 'newline newline)
        (list 'assoc assoc)))

(define (primitive-procedure-name)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-name)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

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


;; lazy part
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)             ; clause from book
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


;;; Representing thunks

;; non-memoizing version of force-it

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

;; thunks

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))


;; memoizing version of force-it

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))


;; A longer list of primitives -- suitable for running everything in 4.2
;; Overrides the list in ch4-mceval.scm

'LAZY-EVALUATOR-LOADED

;(driver-loop)
