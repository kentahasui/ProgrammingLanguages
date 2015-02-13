#lang eopl
;; ====================================================================================
;; Problem 5. 
;; Modify the grammar so that empty operand lists are allowed
;; ====================================================================================


(provide scan&parse
         value-of-aexp-string
         error)

;; The lexical specification
(define the-lexer
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)))
    
;; The grammar
(define the-grammar
  '((aexp (number) const-aexp)
    (aexp
     (arith-op "(" (separated-list aexp ",") ")")
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
      (composite-aexp (operation alist)
                      (cond
                        ;; If the list of a-exps is null, then the
                        ;; operand list is empty. 
                        [(null? alist)
                         (cond
                           [(equal? (op-convert operation) +)
                            0]
                           [(equal? (op-convert operation) *)
                            1]
                           [else
                            (error (op-convert operation))])]
                        ;; If there is only one more element in the list,
                        ;; then end the recursion so we don't get an error message.
                        [(null? (cdr alist))
                         ((op-convert operation)
                          (value-of-aexp (car alist))
                          (if (or (equal? (op-convert operation) +)
                                  (equal? (op-convert operation) -))
                              ;; If the operation is + or -, then return the 
                              ;; additive identity 0. 
                              0
                              ;; Otherwise return the multiplicative identity 1. 
                              1))]
                        ;; Otherwise, there are >1 elements left in the operand list
                        ;; Thus evaluate the expressions
                        [else
                         ;; Call value-of-aexp recursively
                         (value-of-aexp
                          (composite-aexp
                           operation
                           ;; Evaluate the first two numbers in the list of operands
                           ;; Then place it back into the list
                           (cons (const-aexp 
                                  ((op-convert operation)
                                   (value-of-aexp (car alist))
                                   (value-of-aexp (cadr alist))))
                                 (cddr alist))))
                         ])))))

;; error: operation -> error message
;; usage: (error oper)
;; produces: An error message. This function is invoked when 
;;           the "-" or the "/" operator is called with no arguments. 
(define error
  (lambda (oper)
    (eopl:error 'value-of-aexp-acc
           "~s is not associative. 
            You cannot run this operation with no arguments" oper)))