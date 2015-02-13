#lang eopl
;; Kenta Hasui
;; CS235
;; Final Assignment 2014
;;; LET language

(provide (all-defined-out))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    
    (expression (identifier) var-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)
    
    ;; cons-exp
    (expression
     ("cons" "(" expression "," expression ")")
     cons-exp)
    
    ;; emptylist-exp: simply an empty list
    (expression
     ("emptylist")
     emptylist-exp)
    
    ;; car-exp
    (expression
     ("car" "(" expression ")" )
     car-exp)
    
    ;; cdr-exp
    (expression
     ("cdr" "(" expression ")" )
     cdr-exp)
    
    ;; null?-exp
    (expression
     ("null?" "(" expression ")" )
     null?-exp)
    
    
    
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
