#lang eopl
;; Kenta Hasui
;; CS235
;; Final Assignment 2014
;; LET language

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
   
   ))

(define cons-tests
  (test-suite
   "Tests for cons-expressions"
   (check-equal? 
    (run " let x = 4 in cons(x,
            cons(cons(-(x,1),
                    emptylist),
               emptylist))")
    (cons 4 (cons (cons 3 '()) '())) 
    "cons-test 1")
   (check-equal? 
    (run "cons(1, 2)")
    (cons 1 2)
    "simple-cons-pair")
   (check-equal? 
    (run "emptylist")
    '()
    "empty-list-test")
   (check-equal? 
    (run "cons(-(1, 1000), emptylist)")
    '(-999) "one-element-list")
   (check-equal?
    (run "cons(-(7,10), if zero?(-(11, 11)) then 3 else 10)")
    (cons -3 3)
    "diff-and-zero?-exp-in-cons")
   (check-equal? 
    (run "cons(20, cons(let x = 5 in cons(x, 12), emptylist))")
    '(20 (5 . 12))
    "nested-cons")
   (check-exn exn? 
              (lambda () (run "cons(emptylist)"))
              "test-cons-1-exp-exception")
   (check-exn exn?
              (lambda () (run "cons(s1)")
                "test-cons-1-exp-exception2"))
   ))

(define car-tests
  (test-suite
   "Tests for car-expressions"
   (check-equal? (run "car (cons(1, 2))")
                 1 "simple-car-test")
   (check-equal? (run "let x = cons(15, cons(1, 2))
                       in car(x)")
                 15 "let-and-car")
   (check-equal? (run "let z = cons(cons(1, emptylist), emptylist)
                       in car(z)")
                 '(1) "let-and-car-2")
   (check-equal? (run "car(cons(cons(12, cons(19, emptylist)), 16))")
                      '(12 19) "nested-cons")
   (check-equal? (run "car(cons(emptylist, emptylist))")
                 '() "car-of-nested-empty")
   (check-exn exn?
              (lambda () (run "car(emptyList)"))
              "test-car-1-exp-exception")
   (check-exn exn?
              (lambda () (run "car(emptyL"))
              "test-car-1-exp-exception")
   ))

(define cdr-tests
  (test-suite
   "Tests for cdr-expressions"
   (check-equal? (run "cdr(cons(1,2))") 2 "cdr-on-pair")
   (check-equal? (run "cdr(cons(1, emptylist))") '() "cdr-on-1-element-list")
   (check-equal? (run "cdr(cons(1, cons(2, cons(3, cons(4, emptylist)))))")
                 '(2 3 4) "cdr-on-flat-list")
   (check-equal? (run "cdr(cons(5, cons(6, 7)))")
                 '(6 . 7) "cdr-pair")
   (check-equal? (run "let x = cons(1, cons(100, cons(-(20, 20), emptylist)))
                      in cdr(x)")
                 '(100 0) "cdr-let")
   (check-equal? (run "let z = cons(1, let x = cons(7, 4) in cdr(x))
                       in cdr(z)")
                 4 "nested-cdr")
   (check-exn exn? (lambda ()
                     (run "cdr(5)"))
              "test-cdr-exception")
   (check-exn exn? (lambda () (run "cdr(emptyList)"))
              "test-cdr-1-exp-exception")
   (check-exn exn?
              (lambda () (run "car(if zero?(7) then 100 else 30)"))
              "test-cdr-exception2")
   ))

(define null?-tests
  (test-suite
   "Tests for null?-expressions"
   
   (check-equal? (run "null?(emptylist)")
                 #t "simple-null?")
   (check-equal? (run "null?(cdr(cons(emptylist, emptylist)))")
                 #t "simple-null?-2")
   (check-equal? (run "null?(cdr(cons(emptylist, 10)))")
                 #f "simple-false")
   (check-equal? (run "null?(1)")
                 #f "number-false")
   (check-equal? (run "null?(zero?(x))")
                 #f "boolean-false")
   (check-equal? (run "null?(cons(1,2))")
                 #f "not-null-false")
   (check-equal? (run "let k = cons(-(2,200), emptylist)
                      in null?(cdr(k))")
                 #t "let-true")
   
   (check-equal? (run "null?(cdr(cons(1, emptylist)))")
                 #t "simple-true-1")
   (check-equal? (run "let x = cons(1, cons(2, cons(3, emptylist)))
                      in null?(x)")
                 #f "false-not-null")
   (check-equal? (run "let x = cons(1, cons(2, cons(3, emptylist)))
                      in null?(cdr(cdr(cdr(x))))")
                 #t "let-and-null")
   (check-equal? (run "if null?(emptylist) then 10 else 900")
                 10 "if-and-null")
   (check-equal? (run "if null?(cons(1, 300)) then 10 else 900")
                 900 "if-and-not-null")
   ))

(run-tests let-tests 'verbose)
(run-tests cons-tests 'verbose)
(run-tests car-tests 'verbose)
(run-tests cdr-tests 'verbose)
(run-tests null?-tests 'verbose)

