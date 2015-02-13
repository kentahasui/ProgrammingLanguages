#lang eopl

(require "lang.scm")
(require "data-structures.scm")
(require "environments.scm")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : program -> expval

(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (value-of body (init-env))))))

;; value-of : expression * environment -> expval

(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))
      
      (var-exp (id) (apply-env env id))
      
      (diff-exp (exp1 exp2)
                (let ((val1
                       (expval->num
                        (value-of exp1 env)))
                      (val2
                       (expval->num
                        (value-of exp2 env))))
                  (num-val
                   (- val1 val2))))
      
      (minus-exp (exp1)
                 (value-of (diff-exp (const-exp 0)
                                     exp1)
                           env))
      
      (zero?-exp (exp1)
                 (let ((val1 (expval->num (value-of exp1 env))))
                   (if (zero? val1)
                       (bool-val #t)
                       (bool-val #f))))
      
      (if-exp (exp0 exp1 exp2) 
              (if (expval->bool (value-of exp0 env))
                  (value-of exp1 env)
                  (value-of exp2 env)))
      
      (let-exp (id rhs body)       
               (let ((val (value-of rhs env)))
                 (value-of body
                           (extend-env id val env))))
      
      )))
