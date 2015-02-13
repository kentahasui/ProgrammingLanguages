#lang eopl

(define empty-bag
  (lambda ()
    '()))

(define add-to-bag
  (lambda (a s)
    (cons a s)))

(define how-many
  (lambda (a s)
    (cond
      [(null? s) 0]
      [(equal? a (car s))
       (+ 1 (how-many a (cdr s)))]
      [else
       (how-many a (cdr s))])))

(define remove-from-bag
  (lambda (a s)
    (cond
      [(null? s)
       (eopl:error "No occurrences of ~s in this bag" a)]
      [(equal? a (car s))
       (cdr s)]
      [else
       (cons (car s)
             (remove-from-bag a (cdr s)))])))



;; ======================================================================

(define empty-env
  (lambda ()
    '()))

;(define extend-env
;  (lambda (sym val saved-env)
;    (cons (list sym val)
;          saved-env)))
;

(define empty-env? 
  (lambda (env)
    (null? env)))
;
;(define apply-env
;  (lambda (env sym)
;    (cond
;      [(empty-env? env)
;       (eopl:error 'apply-env "no binding for ~s" sym)]
;      [(list? (car env))
;       (let ((first-pair (car env)))
;         (if (eqv? sym (car first-pair))
;             (cadr first-pair)
;             (apply-env (cdr env) sym)))]
;      [else
;       (eopl:error 'apply-env "Bad environment: ~s" env)])))

(define extend-env*
  (lambda (var-list val-list env)
    (cons (list var-list val-list) env)))

(define helper
  (lambda (var-list val-list sym)
    (cond
      [(null? var-list)
       "Null: this symbol is not in the environment"]
      [(equal? (car var-list) sym)
       (car val-list)]
      [else
       (helper (cdr var-list)
               (cdr val-list)
               sym)])))

(define apply-env
  (lambda (env sym)
    (cond
      [(empty-env? env)
       (eopl:error 'apply-env
                   "no binding for ~s in this environment" 
                   sym)]
      [(list? (car env))
       (let* ((binding-list (car env))
             (var-list (car binding-list))
             (val-list (cadr binding-list)))
         (if (string? (helper var-list val-list sym))
             (apply-env (cdr env) sym)
             (helper var-list val-list sym)))]
      [else
       (eopl:error 'apply-env "Bad environment: ~s" env)])))



(define env1 (extend-env* '(a b c) '(1 2 3) (empty-env)))
(define env2 (extend-env* '(x y z) '(100 200 300) env1))
              