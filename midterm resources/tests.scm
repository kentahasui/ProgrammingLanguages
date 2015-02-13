#lang eopl

;; lang3-1

(require rackunit 
         rackunit/text-ui)
(require "environments.scm")
(require "data-structures.scm")  ; for expval constructors
(require "lang.scm")             ; for scan&parse
(require "interp.scm")           ; for value-of-program

;; predicate to test exceptions
;; exn? : any -> bool
(define exn? (lambda (exn) #t))
  
;; run : string -> val
(define run
  (lambda (string)
    (expval->scheme-val (value-of-program (scan&parse string)))))


(define let-tests
  (test-suite
   "tests for LET Language"
   
   ;; simple arithmetic
   (check-equal? (run "11") 11 "positive const")
   (check-equal? (run "-33") -33 "negative const")
   (check-equal? (run "-(44,33)") 11 "simple arith")
   
   ;; nested arithmetic
   (check-equal? (run "-(-(44,33),22)") -11 "nested arith left")
   (check-equal? (run "-(55, -(22,11))") 44 "nested arith right")
   
   ;; simple variables
   (check-equal? (run "x") 10 "test var 1")
   (check-equal? (run "-(x,1)") 9 "test var 2")
   (check-equal? (run "-(1,x)") -9 "test var 3")
         
   ;; simple unbound variables
   (check-exn exn?
              (lambda () (run "foo"))
              "test-unbound-var-foo")
   (check-exn exn? 
              (lambda () (run "-(x,foo)"))
              "test-unbound-var-foo-subtraction")
     
   ;; simple conditionals
   (check-equal? (run "if zero?(0) then 3 else 4") 3 "if true")
   (check-equal? (run "if zero?(1) then 3 else 4") 4 "if false")
   
   ;; test dynamic typechecking
   (check-exn exn?
              (lambda () (run "-(zero?(0),1)"))
              "no-bool-to-diff-1")
   (check-exn exn?
              (lambda () (run "-(1,zero?(0))"))
              "no-bool-to-diff-2")
   (check-exn exn?
              (lambda () (run "if 1 then 2 else 3"))
              "no-int-to-if")
   
   ;; make sure that the test and both arms get evaluated
   ;; properly. 
   (check-equal? 
    (run "if zero?(-(11,11)) then 3 else 4") 
    3 
    "if eval test true")
   (check-equal?
    (run "if zero?(-(11, 12)) then 3 else 4") 
    4 
    "if eval test false")
   
   ;; and make sure the other arm doesn't get evaluated.
   (check-equal?
    (run "if zero?(-(11, 11)) then 3 else foo")
    3
    "if eval test true 2")
   (check-equal?
    (run "if zero?(-(11,12)) then foo else 4")
    4
    "if evalue test false 2")
   
   ;; simple let
   (check-equal? (run "let x = 3 in x") 3 "simple-let-1")
   
   ;; make sure the body and rhs get evaluated
   (check-equal? (run "let x = 3 in -(x,1)") 2 "eval let body")
   (check-equal? (run "let x = -(4,1) in -(x,1)") 2 "eval let rhs")
   
   ;; check nested let and shadowing
   (check-equal? 
    (run "let x = 3 in let y = 4 in -(x,y)")
    -1 
    "simple nested let")
   (check-equal?
    (run "let x = 3 in let x = 4 in x") 
    4
    "check shadowing in body")
   (check-equal?
    (run "let x = 3 in let x = -(x,1) in x")
    2
    "check shadowing in rhs")
   
   ;; check tests for minus
   (check-equal? 
    (run "minus(5)")
    -5
    "check minus operator on a number")
   (check-equal? 
    (run "minus(x)")
    -10
    "(check minus operator on a var-exp)")
   (check-equal? 
    (run "minus(if zero?(0) then 3 else 4)")
    -3
    "check minus on if-exp (true)")
   (check-equal? 
    (run "minus(if zero?(20) then 3 else 4)")
    -4
    "check minus on if-exp (false)")
   (check-equal? 
    (run "minus(-(minus(5),9))")
    14
    "check the minus operator")
   (check-equal? 
    (run "minus(let x = 3 in let x = -(x,1) in x )")
    -2
    "check the minus operator with let")
   
   
   ))

(run-tests let-tests 'verbose)
