#lang eopl
;; Kenta Hasui
;; CS235
;; Final Assignment 2014

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
      
      ;; cons-exp: has 2 expressions as parameters
      (cons-exp (exp1 exp2)
                ;; We define some local variables
                (let* (;; v1 is the expval of exp1
                       (v1 (value-of exp1 env))
                       ;; v2 is the expval of exp2
                       (v2 (value-of exp2 env))
                       ;; We then convert these expvals to SchemeVals
                       (schemeVal1 (expval->scheme-val v1))
                       (schemeVal2 (expval->scheme-val v2)))
                  ;; We now construct a list-val out of two SchemeVals. 
                  ;; This ensures that when we call expVal->SchemeVal 
                  ;; on this list-val (when we call "run"), this expression
                  ;; will return a list of schemeVals rather than a list
                  ;; of expVals. 
                  (list-val (cons schemeVal1 schemeVal2))))
      
      ;; car-exp: has 1 expression as a parameter. Should be a list. 
      (car-exp (expr)
         (let ((expVal1 (value-of expr env))) ;; ExpVal of the expression
           ;; We use cases to extract the list from the list-Val
           (cases expval expVal1
             ;; We return the car of the list. 
             ;; However, the list contains only scheme values, 
             ;; so we must convert it back into expvals. 
             (list-val (lis)
                       (scheme-val->expval (car lis)))
             ;; If the input expression is not a list then we return an error
             (else
              (eopl:error 'value-of
                          "Cannot return car of ~s, as it is not a list"
                          expVal1)))))
      ;; cdr-exp: has 1 argument as parameter. Should be a list
      (cdr-exp (expr)
         (let ((expVal1 (value-of expr env))) ;; ExpVal of the expression
           ;; Again, we extract the list out of the list-val
           (cases expval expVal1
             ;; We get the cdr of the list inside of the list-val.
             ;; Then we place it inside another list-val. 
             (list-val (lis)
                (scheme-val->expval (cdr lis)))
             (else 
              (eopl:error 'value-of
                          "Cannot return cdr of ~s, as it is not a list"
                          expVal1)) 
             )))
      ;; null?-exp: has 1 argument as parameter. Can be anything
      (null?-exp (expr)
        ;; First we convert the expression into an expval
        (let ((expVal1 (value-of expr env)))
          ;; Then we check if the expval is a list-val. 
          (cases expval expVal1
            ;; If it is, we extract the list from the list-val and 
            ;; see if the list is empty. We return a bool-val.
            (list-val (lis)
              (bool-val (empty-list? lis)))
            ;; If the input is not a list, we know it cannot be null. 
            ;; Thus we return false.
            (else
             (bool-val #f)))))
      
      
      
      
      ;; emptylist-exp: Has no parameters.
      ;; simply returns a list-val containing an empty list.
      (emptylist-exp ()
                     (list-val (empty-list)))
      
      
      )))
