#lang eopl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CS235 - Programming Languages
;; Kenta Hasui
;; Fall 2014
;; MP2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide diff-tree?
         get-value
         is-zero? 
         successor
         predecessor
         diff-tree-plus
         diff-tree
         leaf-node
         diff
         one
         zero
         
         scan&parse
         value-of-aexp-string)
         

;; ====================================================================================
;; Problem 1
#| Exercise 2.3 Define a representation of all the integers
    as diff-trees

   Diff-tree ::= (one)
          ::= (diff Diff-tree Diff-tree)
|#
;; ====================================================================================
#|
Exercise 2.3 Define a representation of all the integers
as diff-trees

Diff-tree ::= (one)
          ::= (diff Diff-tree Diff-tree)
|#

(define one 
  (lambda ()
    '(one)))

;; one?: scheme val -> boolean
;; usage: (one? x)
;; produces: returns true if x - '(one), false otherwise 
(define one?
  (lambda (x)
    (eqv? x (one))))

;; Datatype: diff-tree
;; Composed of either a leaf-node (one), or an interior node diff. 
;; Diff consists of two diff-trees. 
(define-datatype diff-tree diff-tree?
  (leaf-node
   (uno one?))
  (diff
   (left diff-tree?)
   (right diff-tree?)))

;; get-value: diff-tree -> int
;; usage: (get-value dtree)
;; produces: the integer that dtree represents. 
(define get-value
  (lambda (dtree)
    (cases diff-tree dtree
      (leaf-node (num) 1)
      (diff (left right)
                     (- (get-value left) (get-value right))))))

#| Part 1. 
As you can see from the three representations of negative 1 below, 
no matter what diff-tree you have, (diff diff-tree (zero)) will 
represent the same integer. We can do this infinitely. Thus there are 
infinite representations of every number in this system. 
|#
 
;; Representation of zero
;; zero: no arguments -> diff-tree
;; usage: (zero)
;; produces: the diff-tree representation of zero. 
(define zero 
    (lambda ()
      (diff (leaf-node (one)) (leaf-node (one)) )))

(define neg1 (diff (zero)
                   (leaf-node (one))))
(define neg1-ver2 (diff neg1 (zero)))
(define neg1-ver3 (diff neg1-ver2 (zero)))

#| Part 2. 
Define zero, is-zero, successor, and predecessor functions
|#

;; For (zero), see above. 

;; is-zero?: diff-tree -> boolean
;; usage: (is-zero? dtree)
;; produces: #t if dtree is a representation of zero, 
;;           #f otherwise. 
(define is-zero?
  (lambda (dtree)
    (= (get-value dtree) 0)))

;; successor: diff-tree -> diff-tree
;; usage: (successor dtree)
;; produces: Another diff-tree that is a representation of 
;;           the value of dtree + 1. 
(define successor
  (lambda (dtree)
    (diff dtree 
          (diff (zero)
                (leaf-node (one))))))

;; predecessor: diff-tree -> diff-tree
;; usage: (predecessor diff-tree)
;; produces: Another diff-tree that is a representation of 
;;           the value of dtree - 1
(define predecessor
  (lambda (dtree)
    (diff dtree (leaf-node (one)))))

#| Part 3. 
Write a procedure diff-tree-plus that does addition
|#

;; diff-tree-plus: diff-tree * diff-tree -> diff-tree
;; usage: (diff-tree-plus dtree1 dtree2)
;; produces: Another diff-tree that is a representation of 
;;           the value of dtree1 + the value of dtree2

(define diff-tree-plus
  (lambda (dtree1 dtree2)
    (let ((additive-inverse (diff (zero) dtree2)))
      (diff dtree1 additive-inverse))))



;; ====================================================================================
;; Problem 2. 
;; Write a grammar for arithmetic expressions
;; ====================================================================================

;; The lexical specification
(define the-lexer
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)))
    
;; The grammar
(define the-grammar
  '((aexp (number) const-aexp)
    (aexp
     (arith-op "(" aexp "," aexp ")")
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

;; ====================================================================================
;; Problem 3. 
;; Write a set of procedures that takes the syntax tree produced by the parser
;; and evaluates it as an arithmetic expression. 
;; ====================================================================================

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
                      ( (op-convert operation)
                        (value-of-aexp left)
                        (value-of-aexp right)
                        )))))
          

