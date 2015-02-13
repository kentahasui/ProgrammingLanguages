#lang eopl

;; data structures for letrec-lang.

(require "lang.scm")                  ; for expression?

(provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; pairs ;;;;;;;;;;;;;;;;;

(define-datatype pair pair?
  (dotted-pair 
   (e1 expression?)
   (e2 expression?)))


;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (pair-val
   (pair pair?))
  (proc-val 
   (proc proc?)))

;;; extractors:

;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

;; expval->pair : ExpVal -> Pair
(define expval->pair
  (lambda (v)
    (cases expval v
      (pair-val (pair) pair)
      (else (expval-extractor-error 'pair v)))))

;; expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (else (expval-extractor-error 'proc v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;; expval->num : ExpVal -> Scheme value,  for testing
(define expval->scheme-val
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (bool-val (bool) bool)
      (proc-val (proc) "procedure value cannot be displayed")
      (else (expval-extractor-error 'TypeError v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pair -> exp1
(define pair->exp1
  (lambda (p)
    (cases pair p
      (dotted-pair (e1 e2)
        e1))))

;; pair -> exp2
(define pair->exp2
  (lambda (p)
    (cases pair p
      (dotted-pair (e1 e2)
        e2))))


;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

;; proc? : SchemeVal -> Bool
;; procedure : Var * Exp * Env -> Proc
(define-datatype proc proc?
  (procedure
   (bvar symbol?)
   (body expression?)
   (env environment?)))

;; Page: 86
(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-rec
   (id symbol?)
   (bvar symbol?)
   (body expression?)
   (saved-env environment?)))

