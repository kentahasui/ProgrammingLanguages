#lang eopl
;; Kenta Hasui
;; CS235
;; Final Assignment 2014

(provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a list.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  ;; The input to a list-val must be a list.
  ;; The list will contain expvals
  (list-val
   (lis is-list?)))

;;; extractors:

(define expval->scheme-val
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (bool-val (bool) bool)
      ;; If the expval is a list-val, we call a helper fuction
      ;; to convert the list of expvals into a list of 
      ;; scheme-vals
      (list-val (lis) (expval->list-helper lis))
      (else (expval-extractor-error 'TypeError v)))))

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

(define expval->list
  (lambda (v)
    (cases expval v
      (list-val (lis) (expval->list-helper lis))
      (else (expval-extractor-error 'list v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;; scheme-val->expval: scheme-val -> expval
;; Converts a number, boolean or a list into the corresponding expVal
;; Returns an error if the input is not one of those types. 
(define scheme-val->expval
  (lambda (x)
    (cond
      [(number? x)
       (num-val x)]
      [(boolean? x)
       (bool-val x)]
      [(is-list? x)
       (list-val x)]
      [else
       (eopl:error 'scheme-val-conversion
                   "exp-vals can only be numbers, booleans or lists")]
      )))

;; expval->list-helper: list-of-expVals -> list-of-scheme-vals
;; Takes in a list of expvals and converts the elements in the list
;; to scheme-vals. 
(define expval->list-helper
  (lambda (listy)
    (cond
      ;; Base case 1: if the list is empty, we return empty
      [(null? listy)
       '()]
      ;; Recursive case: If the list is not empty, and there are 
      ;; still elements inside the list
      [(pair? listy)
       ;; We convert the car of the list to a scheme-val
       ;; and then we recursively call the function on 
       ;; the rest of the list
       (cons (expval->scheme-val (car listy))
             (expval->list-helper (cdr listy)))]
      ;; Base case 2: If the input is NOT a list (i.e. it is a single element
      ;; inside of a cons pair) then we simply return the element converted
      ;; to a scheme-val. 
      [else
       (expval->scheme-val listy)]
      )))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;; example of a data type built without define-datatype

(define empty-env-record
  (lambda () 
    '()))

(define extended-env-record
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define empty-env-record? null?)

(define environment?
  (lambda (x)
    (or (empty-env-record? x)
        (and (pair? x)
             (symbol? (car (car x)))
             (expval? (cadr (car x)))
             (environment? (cdr x))))))

(define extended-env-record->sym
  (lambda (r)
    (car (car r))))

(define extended-env-record->val
  (lambda (r)
    (cadr (car r))))

(define extended-env-record->old-env
  (lambda (r)
    (cdr r)))

;;;;;;;;;;;;;;;;; list structures ;;;;;;;;;;;;;;;;;;;;;;;


;; Empty List
(define empty-list
  (lambda ()
    '()))

;; empty-list?: any value -> boolean
;; Returns true if the input is an empty list
(define empty-list?
  (lambda (x)
    (null? x)))

;; is-list?: any value -> boolean
;; Returns true if the input is a list of anything
;; A list is defined as either a pair of 2 elements or an empty list.
;; Each element can be a list. 
(define is-list?
  (lambda (x)
    (or (pair? x)
        (empty-list? x))))


