#lang eopl
;; ====================================================================================
;; Problem 4. 
;; Extend the grammar for arithmetic expressions
;; ====================================================================================


(provide scan&parse
         value-of-aexp-string)

;; The lexical specification
(define the-lexer
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)))
    
;; The grammar
(define the-grammar
  '((aexp (number) const-aexp)
    (aexp
     (arith-op "(" aexp (arbno "," aexp) ")")
     composite-aexp)
    (arith-op
     ("+")
     plus-op)
    (arith-op
     ("-")
     minus-op)
    (arith-op
     ("*")
     times-op)
    (arith-op
     ("/")
     divide-op)))

(sllgen:make-define-datatypes the-lexer the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:show-define-datatypes the-lexer the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexer the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexer the-grammar))

(define value-of-aexp-string
  (lambda (str)
    (value-of-aexp (scan&parse str))))

;; op-convert: arith-op -> operation
;; usage: (op-convert arop)
;; produces: The arithmetic operations +, -, *, or / depending on 
;;           whether arop is a plus-op, minus-op, times-op, or divide-op. 
(define op-convert
  (lambda (arop)
    (cases arith-op arop
      (plus-op ()
               +)
      (minus-op ()
                -)
      (times-op ()
                *)
      (divide-op ()
                 /))))
  
;; value-of-aexp: aexp -> number
;; usage: (value-of-aexp arith-exp)
;; produces: Evaluates the syntax tree represented by aexp as an arithmetic expression
;;           Returns a number. 

(define value-of-aexp
  (lambda (arith-exp)
    (cases aexp arith-exp
      (const-aexp (num)
                  num)
      (composite-aexp (operation left right)
                      ;; if the list-of aexp is empty
                      (if (null? right)
                          ;; Evaluate the expression from left to right
                          ((op-convert operation)
                           (value-of-aexp left)
                           (cond
                             ;; If the operation is + or -, 
                             ;; return the additive identity (0)
                             [(or (equal? (op-convert operation) +)
                                  (equal? (op-convert operation) -))
                              0]
                             ;; If the operation is * or -, 
                             ;; return the multiplicative identity (1)
                             [else 
                              1]))
                          ;; If the list-of aexp isn't empty
                          (value-of-aexp 
                           ;; Create a new composite-aexp out of: 
                           ;; the operation, the result of evaluating the 
                           ;; first two numbers, and the rest of the list
                           (composite-aexp operation
                                           (const-aexp 
                                            ((op-convert operation)
                                             (value-of-aexp left)
                                             (value-of-aexp (car right))))
                                           (cdr right)))
                          )))))