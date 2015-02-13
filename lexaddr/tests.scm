#lang eopl
;; Kenta Hasui
;; CMPU 235

;; LEXADDR language   

;; exception tests not working; search for "DN" below

(require rackunit 
         rackunit/text-ui)
(require "environments.scm")
(require "data-structures.scm")  ; for expval constructors
(require "lang.scm")             ; for scan&parse
(require "interp.scm")           ; for value-of-program
(require "translator.scm")       ; for translation-of-program

;; predicate to test exceptions
;; exn? : any -> bool
(define exn? (lambda (exn) #t))

;; run : String -> ExpVal
(define run
  (lambda (string)
	 (expval->scheme-val
	  (value-of-translation 
		(translation-of-program (scan&parse string))))))

(define proc-tests         ;; adapted from eopl tests
  (test-suite
   "tests for PROC language"

    (check-equal? (run "(proc(x) -(x,1)  30)") 
                  29 "apply-proc-in-rator-pos")

    (check-equal? (run "let f = proc (x) -(x,1) in (f 30)") 
                  29 "apply-simple-proc")

    (check-equal? (run "(proc(f)(f 30)  proc(x)-(x,1))") 
                  29 "let-to-proc-1")

    (check-equal? (run "((proc (x) proc (y) -(x,y)  5) 6)") 
                  -1 "nested-procs")

    (check-equal? (run "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)")
                  -1 "nested-procs2")

    (check-equal? (run "
let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in let
    t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)" )
12 "y-combinator-1")

))



(define let-tests  ;; modified to check translator exceptions
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
         
   ;; simple unbound variables; exception is now at translation time
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
   ;(no-bool-to-diff-1 "-(zero?(0),1)" error)
   ;(no-bool-to-diff-2 "-(1,zero?(0))" error)
   ;(no-int-to-if "if 1 then 2 else 3" error)
   
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
   (check-exn exn?
     (lambda () (run "if zero?(-(11, 11)) then 3 else foo"))
    "if eval test true 2")
   (check-exn exn?
    (lambda () (run "if zero?(-(11,12)) then foo else 4"))
    "if eval test false 2")
   
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

(define pair-tests
  (test-suite
   "Tests for pair and unpair"
   
   ;; Simple pairs-test
   (check-equal? (run "let z = pair(3,4)
                       in unpair z as (x,y)
                          in -(y,x)")
                 1
                 "pair/unpair 1")
   (check-equal? (run "let z = pair(3,
                                    pair(proc (u) proc (v) -(u,v),
                                         4))
                       in unpair z as (u,p2)
                         in unpair p2 as (f,v)
                          in ((f u) v)")
                 -1
                 "pair/unpair 2")
   (check-equal? (run "unpair pair(3,4) as (k,h) in -(h,k)")
                 1 "unpair-pair-not-identifier")
   (check-equal? (run "let z = pair(3, pair(proc (u) proc (v) -(u, v), 4))
                       in unpair z as (u, p2) in unpair p2 as (f, v)
                       in ((f u) v)")
                 -1 "nested-unpairs")
   (check-exn exn? (lambda () (run "let k = 10 in unpair k as (a, b) in +(a, b)"))
              "unpair-int-error")
   (check-exn exn? (lambda () (run "unpair zero?(0) as (x, y) in *(x, y)"))
              "unpair-bool-error")
   
   ;; Tests to make sure the order of the variables are maintained: 
   ;;   a-b != b-a
   (check-equal? (run "unpair pair(10, 20) as (a, b) in -(a,b)")
                 -10 "keep-order-1")
   (check-equal? (run "unpair pair(10, 20) as (a, b) in -(b, a)")
                 10 "keep-order-2")
   (check-equal? (run "let k = pair(10, proc(k) -(k, 1000)) 
                        in unpair k as (a, k)
                        in (k a)")
                 -990 "proc-and-let")
   
   ))

(run-tests let-tests 'verbose)

(run-tests proc-tests 'verbose)

(run-tests pair-tests 'verbose)
