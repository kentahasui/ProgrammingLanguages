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

        (div-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (/ num1 num2)))))

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
              (if (equal? num1 num2)
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
              
        (greater?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (if (> num1 num2)
                (bool-val #t)
                (bool-val #f)))))

        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        (cond-exp (lhs-exps rhs-exps)
          (value-of-cond lhs-exps rhs-exps env))

        (allof-exp (exp1 exp-list)
          (let ((val1 (value-of exp1 env)))
            (bool-val 
             (and val1
                  (value-of-allof exp-list env)))))
        
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

        (pair-exp (e1 e2)
          (pair-val (dotted-pair e1 e2)))
        
        (unpair-exp (var x y exp1)
          (let ((p1 (apply-env env var)))
            (let ((pair-first (pair->exp1 (expval->pair p1)))
                  (pair-second (pair->exp2 (expval->pair p1))))
              (let ((val-first (value-of pair-first env))
                    (val-second (value-of pair-second env)))
                (value-of exp1
                  (extend-env x val-first
                    (extend-env y val-second
                      env)))))))
        
        )))

  ;; value-of-cond : (listof ExpVal) (listof ExpVal) Env -> ExpVal
  (define value-of-cond
    (lambda (lhsides rhsides env)
      (cond [(null? lhsides) 
             (eopl:error 'value-of-cond "No tests succeeded")]
            [else
             (let ((lhs-val (value-of (car lhsides) env))
                   (rhs-val (value-of (car rhsides) env)))
               (if (expval->bool lhs-val)
                   rhs-val
                   (value-of-cond (cdr lhsides) (cdr rhsides) env)))])))
    
  ;; value-of-allof : (listof ExpVal) Env -> ExpVal
  (define value-of-allof
    (lambda (exp-list env)
      (cond [(null? exp-list) #t] 
            [else
             (let ((exp-val (value-of (car exp-list) env)))
               (and (expval->bool exp-val)
                   (value-of-allof (cdr exp-list) env)))])))
    
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of body (extend-env var arg saved-env))))))
  


  
