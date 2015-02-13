#lang eopl
;; Kenta Hasui
;; CMPU 235

  (require "lang.scm")

  (provide translation-of-program)
  ;;;;;;;;;;;;;;;; lexical address calculator ;;;;;;;;;;;;;;;;

  ;; translation-of-program : Program -> Nameless-program
  ;; Page: 96
  (define translation-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (a-program                    
            (translation-of exp1 (init-senv)))))))

  ;; translation-of : Exp * Senv -> Nameless-exp
  ;; Page 97
  (define translation-of
    (lambda (exp senv)
      (cases expression exp
        (const-exp (num) (const-exp num))
        (diff-exp (exp1 exp2)
          (diff-exp
            (translation-of exp1 senv)
            (translation-of exp2 senv)))
        (zero?-exp (exp1)
          (zero?-exp
            (translation-of exp1 senv)))
        (if-exp (exp1 exp2 exp3)
          (if-exp
            (translation-of exp1 senv)
            (translation-of exp2 senv)
            (translation-of exp3 senv)))
        (var-exp (var)
          (nameless-var-exp
            (apply-senv senv var)))
        (let-exp (var exp1 body)
          (nameless-let-exp
            (translation-of exp1 senv)            
            (translation-of body
              (extend-senv var senv))))
        (proc-exp (var body)
          (nameless-proc-exp
            (translation-of body
              (extend-senv var senv))))
        (call-exp (rator rand)
          (call-exp
            (translation-of rator senv)
            (translation-of rand senv)))
        
        (pair-exp (e1 e2)
          ;; Converts a pair-exp into a nameless pair-exp
          ;; Removes the variables of the expressions inside the pair
          (pair-exp (translation-of e1 senv)
                    (translation-of e2 senv)))
        
        
        #| NOTE: In this implementation, since the static environment is a flat 
           list of symbols/identifiers, I have decided to place the two variables
           in the list at DIFFERENT DEPTHS. In the body of the unpair-expression, 
           the variable (s1) that represents the first element in the pair has
           a depth of 0, while the variable (s2) that represents the second 
           element in the pair has a depth of 1. 
           Another implementation for this could add both variables as a cons 
           cell in the list. However, we would need to pass the position as well 
           as the depth our nameless-var-exp (which would require us to change 
           the grammar and translator as well as the interpreter). 
           
        |#
        (unpair-exp (p s1 s2 b)
          (nameless-unpair-exp
           ;; Converts the pair-exp into a nameless pair-exp
           (translation-of p senv)
           ;; Then translates the body, adding the two variables 
           ;; into the static environment. 
           ;; We add s2 first and then s1, giving the first symbol a depth of 0
           ;; and the second symbol a depth of 1. 
           (translation-of b
                           (extend-senv s1 (extend-senv s2 senv)))))
           
                    
        
        (else (report-invalid-source-expression exp))
        )))

  (define report-invalid-source-expression
    (lambda (exp)
      (eopl:error 'value-of 
        "Illegal expression in source code: ~s" exp)))
  
   ;;;;;;;;;;;;;;;; static environments ;;;;;;;;;;;;;;;;
  
  ;;; Senv = Listof(Sym)
  ;;; Lexaddr = N

  ;; empty-senv : () -> Senv
  ;; Page: 95
  (define empty-senv
    (lambda ()
      '()))

  ;; extend-senv : Var * Senv -> Senv
  ;; Page: 95
  (define extend-senv
    (lambda (var senv)
      (cons var senv)))
  
  ;; apply-senv : Senv * Var -> Lexaddr
  ;; Page: 95
  (define apply-senv
    (lambda (senv var)
      (cond
        ((null? senv) (report-unbound-var var))
        ((eqv? var (car senv))
         0)
        (else
          (+ 1 (apply-senv (cdr senv) var))))))

  (define report-unbound-var
    (lambda (var)
      (eopl:error 'translation-of "unbound variable in code: ~s" var)))

  ;; init-senv : () -> Senv
  ;; Page: 96
  (define init-senv
    (lambda ()
      (extend-senv 'i
        (extend-senv 'v
          (extend-senv 'x
            (empty-senv))))))
  

