#lang eopl
  
  ;; interpreter for the LETREC language.  

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 83
  (define value-of
    (lambda (exp env)
      (cases expression exp

        (const-exp (num) (num-val num))

        (var-exp (var) (apply-env env var))

        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
        
        (add-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (+ num1 num2)))))
        
        (mult-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (* num1 num2)))))
        
        (quot-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (quotient num1 num2)))))

        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
        
        (equal?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (if (= num1 num2)
                  (bool-val #t)
                  (bool-val #f)))))
        
        (greater?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (if (> num1 num2)
                  (bool-val #t)
                  (bool-val #f)))))
        
        (less?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (if (< num1 num2)
                  (bool-val #t)
                  (bool-val #f)))))
        
        (not-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((bool1 (expval->bool val1)))
              (bool-val (not bool1)))))
              
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of letrec-body
            (extend-env-rec p-name b-var p-body env)))
        
        (cond-exp (clause-list body-list)
          (cond
            ;; If the list of clauses is empty, none of the clauses was true.
            ;; Return error message. 
            [(null? clause-list)
             (eopl:error 'value-of "None of the clauses evaluate to true")]
            [else
             ;; Otherwise, we evaluate the first clause. 
             (let ((first-clause (car clause-list)) ;; First clause
                   (first-body (car body-list)))    ;; First body
               ;; Find the expval of the first clause
               (let ((first-val (value-of first-clause env)))
                 ;; Convert the expval to a boolean
                 (let ((first-bool (expval->bool first-val)))
                   ;; Check if the first clause evaluates to true.
                   (if first-bool
                       ;; If the first clause is true, evaluate the first body
                       (value-of first-body env)
                       ;; If the first clause is false, recursively call value-of
                       ;; on the rest of both lists. 
                       (value-of (cond-exp (cdr clause-list)
                                           (cdr body-list))
                                 env)))))]))
        
        (allof-exp (exp1 exp-list) ;; takes in an expression and a list of expressions
          (let* (;; Expval of first expression
                 (exp-val1 (value-of exp1 env))
                 ;; Boolean value of first expression
                 (bool1 (expval->bool exp-val1)))
            (cond
              ;; If the first expression is false, return false immediately. Do not 
              ;; evaluate anything else. 
              [(equal? #f bool1) (bool-val #f)]
              ;; If the list of expressions is empty, then there are no more expressions to 
              ;; evaluate. We know that exp1 evaluates to #t, so return true. 
              [(null? exp-list)
                (bool-val #t)]
              ;; Otherwise exp1 is true and there are more expressions to evaluate. 
              ;; We recursively call value-of on the rest of the exp-list. 
              [else 
               ;; We let next-exp be the next expression to evaluate in exp-list
               ;; We let rest-list be the rest of the exp-list
               (let ((next-exp (car exp-list))
                     (rest-list (cdr exp-list)))
                 ;; Recursively call value-of
                 (value-of (allof-exp next-exp rest-list)
                           env))])))
        
        ;; If expression is a pair-exp, 
        (pair-exp (exp1 exp2)
          ;; Create a pairs-val from the input
          (pairs-val (pair 
                      (value-of exp1 env)
                      (value-of exp2 env))))
        
        ;; If expression is an unpair-exp
         (unpair-exp (e1 sym1 sym2 e2)
          ;; First find the exp-val of e1. Store this as pair1
          (let ((pair1 (value-of e1 env)))
            (cases expval pair1
              ;; If pair1 is indeed a pairs-value, do some work. 
              (pairs-val (p)
                ;; We extract the two expressions from the pair. 
                ;; v1 and v2 are both expvals. 
                (let* ((v1 (unpair-first p))
                       (v2 (unpair-second p)))
                  ;; Then we find the value of the body (e2), but with the new 
                  ;; environment in which sym1 is bound to v1 and sym2 is bound to v2. 
                  (value-of e2
                            (extend-env sym2 
                                        v2
                                        (extend-env sym1 v1 env)))))
                  
              ;; Otherwise return an error message. 
              (else 
               (eopl:error 'value-of "The value of ~s is not a pair-exp" e1))))) 
                 
                   

        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal

  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of body (extend-env var arg saved-env))))))
  


  
