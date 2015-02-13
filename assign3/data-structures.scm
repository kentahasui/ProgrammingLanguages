#lang eopl

  ;; data structures for letrec-lang.

  (require "lang.scm")                  ; for expression?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.
;;; Or a pairs-val

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    (pairs-val
     (pairs pairs?)))

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

  ;; expval->proc : ExpVal -> Proc
  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  ;; expval->pairs: ExpVal -> Pairs
  (define expval->pairs
    (lambda (v)
      (cases expval v
        (pairs-val (pair) pair)
        (else (expval-extractor-error 'pair v)))))

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
        (pairs-val (pair) pair)
        (else (expval-extractor-error 'TypeError v)))))


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


;;;;;;;;;;;;;;;;; pairs ;;;;;;;;;;;;;;;;;;;;;;

;; A pair consists of 2 expressed values
(define-datatype pairs pairs?
  (pair 
   (val1 expval?)
   (val2 expval?)))

;; unpair-first: Pairs -> ExpVal
;; Returns the first element in the pair
(define unpair-first
  (lambda (p)
    (cases pairs p
      (pair (v1 v2)
        v1))))

;; unpair-second: Pairs -> ExpVal
;; Returns the second element in the pairs
(define unpair-second
  (lambda (p)
    (cases pairs p
      (pair (v1 v2)
        v2))))


