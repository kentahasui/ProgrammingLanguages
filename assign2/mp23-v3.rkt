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
     (arith-op "(" aexp (arbno "," aexp) ")")
     composite-aexp)
    (aexp
     ()
     empty-aexp)
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
    (letrec (;; value-of-aexp-acc: aexp * procedure -> number
             ;; usage: (value-of-aexp-acc arith-exp op-acc)
             ;; produces: This function uses an acculmulator to hold the 
             ;;   operator (+, -, *, or /)
             (value-of-aexp-acc
              (lambda (arith-exp op-acc)
                (cases aexp arith-exp
                  (const-aexp (num)
                              num)
                  (composite-aexp (operation left right)
                                  ; if the list-of aexp is empty
                                  (let ((converted-operation (op-convert operation)))
                                    (cond
                                      [(null? right)
                                       ;; Evaluate the expression from left to right
                                       (converted-operation
                                        (value-of-aexp-acc left converted-operation)
                                        (cond
                                          ;; If the operation is + or -, 
                                          ;; return the additive identity (0)
                                          [(or (equal? converted-operation +)
                                               (equal? converted-operation -))
                                           0]
                                          ;; If the operation is * or -, 
                                          ;; return the multiplicative identity (1)
                                          [else 
                                           1]))]
                                      ;; If the list-of aexp isn't empty
                                      [(value-of-aexp-acc
                                        ;; Create a new composite-aexp out of: 
                                        ;; the operation, the result of evaluating the 
                                        ;; first two numbers, and the rest of the list
                                        (composite-aexp operation
                                                        (const-aexp 
                                                         (converted-operation
                                                          (value-of-aexp-acc left converted-operation)
                                                          (value-of-aexp-acc (car right) converted-operation)))
                                                        (cdr right))
                                        converted-operation
                                        )]
                                      )))
                  (empty-aexp ()
                              (cond
                                [(equal? op-acc +)
                                 0]
                                [(equal? op-acc *)
                                 1]
                                [else 
                                 (error op-acc)]))))))
      ;; calls value-of-aexp-acc with a generic operation, +
      (value-of-aexp-acc arith-exp +)
      )))

;; error: operation -> error message
;; usage: (error oper)
;; produces: An error message. This function is invoked when 
;;           the "-" or the "/" operator is called with no arguments. 
(define error
  (lambda (oper)
    (eopl:error 'value-of-aexp-acc
           "~s is not associative. 
            You cannot run this operation with no arguments" oper)))