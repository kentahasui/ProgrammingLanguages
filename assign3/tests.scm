#lang eopl

;; LETREC language 

(require rackunit 
         rackunit/text-ui)
(require "environments.scm")
(require "data-structures.scm")  ; for expval constructors
(require "lang.scm")             ; for scan&parse
(require "interp.scm")           ; for value-of-program

;; predicate to test exceptions
;; exn? : any -> bool
(define exn? (lambda (exn) #t))
  
;; run : String -> ExpVal
(define run
  (lambda (string)
    (expval->scheme-val (value-of-program (scan&parse string)))))

(define letrec-tests  
  (test-suite
   "tests for LETREC language"

       ;; simple letrecs
      (check-equal? (run 
        "letrec f(x) = -(x,1) in (f 33)") 
        32 "simple-letrec-1")
      (check-equal? (run 
        "letrec f(x) = if zero?(x)  then 0 else -((f -(x,1)), -2) in (f 4)")  
         8 "simple-letrec-2")
      (check-equal? (run 
        "let m = -5 in letrec f(x) = if zero?(x) then 0 else -((f -(x,1)), m) in (f 4)") 
        20 "simple-letrec-3")

;; alas, no multiplication in this language.  Exercise: define
;; multiplication as a letrec and then use it to define factorial.
      (check-equal? (run  "letrec
                           fact(x) = if zero?(x) then 1 else *(x, (fact -(x, 1)))
                           in (fact 6)" )
                  720 "fact-of-6")

      ;; higher order nested letrecs       
      (check-equal? (run
        "letrec even(odd) = proc(x) if zero?(x) then 1 else (odd -(x,1))
         in letrec  odd(x)  = if zero?(x) then 0 else ((even odd) -(x,1))
         in (odd 13)")
         1
        "HO-nested-letrecs")
))

(define proc-tests     
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
   (check-exn exn? (lambda () (run "-(zero?(0),1)")) "subtract-boolean-error")
   (check-exn exn? (lambda () (run "-(1,zero?(0))")) "subtract-boolean-error-2")
   (check-exn exn? (lambda () (run "if 1 then 2 else 3")) "if-int-error")
   
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

(define arith-tests
  (test-suite
   "Tests for 3.7: Arithmetic operators"
   
   ;; simple addition
   (check-equal? (run "+(10, 5)") 15 "simple-addition 1")
   (check-equal? (run"+(0, 0)") 0 "simple-addition 2")
   (check-equal? (run "+(1, +(2, 3))") 6 "simple-addition 3")
   (check-equal? (run "+(0,10)") 10 "add-0")
   (check-equal? (run "+(10,0)") 10 "add-0-2")
   (check-equal? (run "+(+(7, 3), 10)") 20 "nested-add left")
   (check-equal? (run "+(-(7, 2), -(2, 1))") 6 "addition and subtraction")
   (check-equal? (run "+(-(2, 4), -(7, 10))") -5 "add negative numbers")
   ;; Simple variables
   (check-equal? (run "+(x,9)") 19 "test var 1")
   ;; If statements
   (check-equal? (run "if zero?(0) then +(7, x) else +(4, 0)") 17 "if true")
   (check-equal? (run "if zero?(+(7, 20)) then +(7, x) else +(4, 0)") 4 "if false")
   ;; Simple let
   (check-equal? (run "let x = +(4,1) in +(x,1)") 6 "eval let addition")
   ;; Proc
   (check-equal? (run "let f = proc (x) +(x,1) in (f 30)") 
                  31 "apply-simple-proc-add")
   ;; Letrec
   (check-equal? (run 
        "letrec f(x) = if zero?(x) then 0 else +((f -(x,1)), -2) in (f 4)")  
         -8 "simple-letrec-2")
   
   ;; simple multiplication
   (check-equal? (run "*(10, 5)") 50 "simple-mult 1")
   (check-equal? (run "*(0, 5)") 0 "mult-0 1")
   (check-equal? (run "*(5, 0)") 0 "mult-0 2")
   (check-equal? (run "*(-(0,2), +(9, 1))") -20 "mult-neg")
   (check-equal? (run "-(*(10, 3), *(5, 2))") 20 "inner mults")
   (check-equal? (run "+(*(20, *(2, 3)), -(*(10, 2), +(17, 1)))") 122 "nested mult")
   ;; Simple variables
   (check-equal? (run "*(x,9)") 90 "test var 1")
   ;; If statements
   (check-equal? (run "if zero?(*(20, 0)) then *(7, x) else +(4, 0)") 70 "if true")
   (check-equal? (run "if zero?(*(7, 20)) then *(7, x) else *(4, 0)") 0 "if false")
   ;; Simple let
   (check-equal? (run "let x = *(4,3) in *(x,2)") 24 "eval let addition")
   ;; Proc
   (check-equal? (run "let f = proc (x) *(x,2) in (f 30)") 
                  60 "apply-simple-proc-add")
   
   ;; simple integer quotient
   (check-equal? (run "*(10, 5)") 50 "simple-mult 1")
   (check-equal? (run "*(0, 5)") 0 "mult-0 1")
   (check-equal? (run "*(5, 0)") 0 "mult-0 2")
   (check-equal? (run "*(-(0,2), +(9, 1))") -20 "mult-neg")
   (check-equal? (run "-(*(10, 3), *(5, 2))") 20 "inner mults")
   (check-equal? (run "+(*(20, *(2, 3)), -(*(10, 2), +(17, 1)))") 122 "nested mult")
   ;; Simple variables
   (check-equal? (run "*(x,9)") 90 "test var 1")
   ;; If statements
   (check-equal? (run "if zero?(*(20, 0)) then *(7, x) else +(4, 0)") 70 "if true")
   (check-equal? (run "if zero?(*(7, 20)) then *(7, x) else *(4, 0)") 0 "if false")
   ;; Simple let
   (check-equal? (run "let x = *(4,3) in *(x,2)") 24 "eval let mult")
   ;; Proc
   (check-equal? (run "let f = proc (x) *(x,2) in (f 30)") 
                  60 "apply-simple-proc-mult")
   
   ;; simple quotient
   (check-equal? (run "/(10, 5)") 2 "simple-quotient 1")
   (check-equal? (run "/(9, 2)") 4 "simple-quotient 2")
   (check-equal? (run "/(0, 5)") 0 "quot-0")
   (check-equal? (run "/(-(0,10), +(9, 0))") -1 "quot-neg 1")
   (check-equal? (run "/(*(7, 10), -(0, 6))") -11 "quot-neg 2")
   (check-equal? (run "+(/(20, /(3, 2)), -(/(10, 1), +(5, 0)))") 25 "nested-quot")
   (check-exn exn? (lambda () (run "/(90, 0)")) "divide by zero-error")
   
   ;; Simple variables
   (check-equal? (run "/(x,3)") 3 "test var 1")
   ;; If statements
   (check-equal? (run "if zero?(/(0, -4)) then /(7, x) else /(4, 2)") 0 "if true")
   (check-equal? (run "if zero?(/(20, 3)) then /(7, x) else /(4, 2)") 2 "if false")
   ;; Simple let
   (check-equal? (run "let x = /(50,2) in /(x,5)") 5 "eval-let-quotient")
   ;; Proc
   (check-equal? (run "let f = proc (x) /(x,2) in (f 30)") 
                  15 "apply-simple-proc-quotient")
   ;; Letrec
   (check-equal? (run 
        "letrec f(x) = if zero?(-(x, 1)) then 0 else +((f /(x,2)), 2) in (f 4)")  
         4 "simple-letrec-quot")
      ))

(define comparison-tests
  (test-suite
   "Tests for 3.8: Comparison operators"
   
   ;; Simple equals
   (check-equal? (run "equal?(5, 5)") #t "simple-equal 1")
   (check-equal? (run "equal?(0, 5)") #f "zero-equal")
   (check-equal? (run "equal?(*(20, 2), /(40, 1))") #t "simple-equal 2")
   (check-equal? (run "equal?(+(15, 5), -(30, 2))") #f "simple-equal 3")
   ;; If
   (check-equal? (run "if equal?(*(x, 10), 100) then +(1, 2) else equal?(x, 10)")
                 3 "if true")
   (check-equal? (run "if equal?(*(x, 7), 100) then +(1, 2) else equal?(x, 10)")
                 #t "if false")
   ;; Simple let
   (check-equal? (run "let z = equal?(x, 7) in if z then 0 else 1") 1 "eval-let-equal")
   ;; Proc
   (check-equal? (run "let y = proc (z) equal?(z, x) in (y 20)")
                 #f "proc-equal")
   (check-equal? (run "let y = proc (z) equal?(z, x) in (y 10)")
                 #t "proc-equal-true")
   
   ;; Simple greater
   (check-equal? (run "greater?(5, 5)") #f "equals")
   (check-equal? (run "greater?(0, 5)") #f "less-than")
   (check-equal? (run "greater?(2000, 7)") #t "greater-1")
   (check-equal? (run "greater?(0, -72)") #t "greater-2")
   (check-equal? (run "greater?(*(20, 2), /(40, 1))") #f "equals-2")
   (check-equal? (run "greater?(+(70, 5), -(30, 2))") #t "greater-3")
   
   ;; Simple less?
   (check-equal? (run "less?(5, 5)") #f "equals")
   (check-equal? (run "less?(0, 5)") #t "less-1")
   (check-equal? (run "less?(2000, 7)") #f "greater-1")
   (check-equal? (run "less?(-20, 0)") #t "less-2")
   (check-equal? (run "less?(*(20, 2), /(40, 1))") #f "equals-2")
   (check-equal? (run "less?(+(70, 5), -(30, 2))") #f "greater-2")
   (check-equal? (run "less?(+(9, -(15, 14)), /(1000, *(10, 2)))") #t "less-3")
   
   ;; Simple not? 
   (check-equal? (run "not(zero?(0))") #f "not-true")
   (check-equal? (run "not(equal?(10, 7))") #t "not-false")
   (check-equal? (run "not(greater?(10, 2))") #f "not-true-2")
   (check-equal? (run "not(less?(*(x, 2), /(x, 5)))") #t "not-false-2")
   ;; Simple if
   (check-equal? (run "if not(greater?(x, 200)) then i else v") 1 "if-not-false")
   (check-equal? (run "if not(greater?(200, x)) then i else v") 5 "if-not-true")
   ))

(define cond-tests
  (test-suite
   "Tests for 3.12: cond"
   ;; Simple cond
   (check-equal? (run "cond zero?(0) ==> 4 end") 4 "cond-1")
   (check-equal? (run "cond zero?(15) ==> 3 greater?(x, -3) ==> less?(3, 1) end") 
                 #f "cond-bool-body")
   (check-exn exn? (lambda () (run "cond end")) "no-clauses-error")
   (check-exn exn? (lambda () (run "cond zero?(10) ==> 1 equal?(0,100) ==> 2 end")) "none-true-error")
   ;; Cond with if
   (check-equal? (run "cond not(zero?(0)) ==> *(3, 10) greater?(x, 33) ==> less?(3, 1) 
                  less?(10, 20) ==> if zero?(7) then 3 else 65 end")
                 65 "cond-if-body")
   ;; Cond with let
   (check-equal? (run "let z = 20 in let f = proc (j) *(j, 5) in cond zero?(10) ==> (f 12) 
                       greater?((f z), 1) ==> 7 less?((f z), 300) ==> 8 end")
                 7 "cond-in-let")
   (check-equal? (run  "cond zero?(0) ==> let k = 12 in k greater?(12, 0) ==> 9 
                        not(zero?(7)) ==> 100 less?(10, 100000) ==> 8 end")
                 12 "return-first-body")
   (check-equal? (run "cond zero?(0) ==> 100 *(k, 10) ==> *(m, 90) end")
                 100 "short-circuit")
   ))

(define allof-tests
  (test-suite
   "Tests for allof"
   
   ;; Simple allof
   (check-equal? (run "allof zero?(x) end") #f "allof-1")
   (check-equal? (run "allof zero?(-(i, 1)) end") #t "allof-2")
   (check-equal? (run "allof not(zero?(x)) and greater?(v,i) end") #t "allof-3")
   (check-equal? (run "allof greater?(2, 7) and zero?(-(zaaa,1)) end") #f "allof-shortcut")
   (check-equal? (run "allof less?(-10, 2) and not(zero?(7)) and equal?(*(7, 2), +(4, x)) end")
                 #t "allof-4")
   (check-exn exn? (lambda () (run "allof end")) "no-expression-error")
   
   ;; if-allof
   (check-equal? (run "if allof zero?(0) end then 3 else 4") 3 "if-allof-true")
   (check-equal? (run "if allof not(zero?(-(x, 10))) end then 3 else 4") 4 "if-allof-false")
   (check-equal? (run "if allof greater?(2, 0) and less?(0, 10) end then i else v")
                 1 "if-allof-true-2")
   (check-equal? (run "if allof zero?(0) and greater?(8, 100) and less?(3, zzz) end
                       then 100 else 200")
                 200 "if-allof-false-3")
   
   ;; let-allof
   (check-equal? (run  "let x = 0 in let y = 1 in if allof zero?(x) and zero?(y)
                        end then 33 else 44")
                 44 "let-allof-1")
   (check-equal? (run "let f = proc (x) aha
                  in if allof zero?(1) and (f 1) end then 3 else 4")
                 4 "let-allof-2")
   (check-equal? (run "let g = proc (k) *(k, x) 
                       in if allof zero?(0) and greater?((g 20), 70) end then 
                       let z = 900 in *(2, z) else 8")
                 1800 "let-allof-3")
   (check-equal? (run "let g = proc (k) *(k, x) 
                       in if allof zero?(0) and less?((g 20), 70) end then 
                       let z = 900 in *(2, z) else 8")
                 8 "let-allof-4")
   
   ;; cond-allof
   (check-equal? (run "cond allof zero?(0) and equal?(2, 2) end ==> 3 end")
                 3 "cond-allof-1")
   (check-equal? (run "cond allof zero?(0) and greater?(2, 2) end ==> 3 
                       allof not(zero?(7)) end ==> /(10, 9) end")
                 1 "cond-allof-2")
   
   ))

(define pairs-test
  (test-suite
   "Tests for pair and unpair"
   
   ;; Simple pair-unpair with procs
   (check-equal? (run "let z = pair(3,4) in unpair z as (x,y) in -(y, x)")
                 1 "unpair-identifier")
   (check-equal? (run "unpair pair(3,4) as (k,h) in -(h,k)")
                 1 "unpair-pair-not-identifier")
   (check-equal? (run "let z = pair(3, pair(proc (u) proc (v) -(u, v), 4))
                       in unpair z as (u, p2) in unpair p2 as (f, v)
                       in ((f u) v)")
                 -1 "nested-unpairs")
   (check-equal? (run "let k = pair(proc (j) -(j, x), 100) in unpair k as (j, z)
                       in (j (j z))")
                 80 "proc-pair")
   
   (check-exn exn? (lambda () (run "let k = 10 in unpair k as (a, b) in +(a, b)"))
              "unpair-int-error")
   (check-exn exn? (lambda () (run "unpair zero?(0) as (x, y) in *(x, y)"))
              "unpair-bool-error")
   
   ;; Simple arithmetic in pairs
   (check-equal? (run "unpair pair(10, -2) as (i, v) in *(v, /(10, +(i, -(i,i))))")
                 -2 "arith-in-body")
   (check-equal? (run "unpair pair(*(10, 7), +(7,3)) as (x,y) in
                      if greater?(x, y) then x else y")
                 70 "arith-in-pair")
   (check-equal? (run "let x = pair(7, /(7,3)) in unpair x as (x,y)
                       in *(x, y)")
                 14 "arith-in-pair-2")
   
   ;; Pairs in cond
   (check-equal? (run "let z = pair(2, 4) in unpair z as (k, w) in
                       cond greater?(k, w) ==> z equal?(k, -(w, k)) ==> 100 end")
                 100 "cond-pair-1")
   (check-equal? (run "unpair pair(20, 0) as (k, z) in less?(z, k)")
                 #t "bool-test")
   (check-equal? (run "cond unpair pair(20, 0) as (k,z) in less?(z, k) ==> +(-20, 40) 
                       not(zero?(3)) ==> 2 end")
                      20 "cond-pair-2")
   (check-equal? (run "let z = pair(cond zero?(10) ==> 10 equal?(2,2) ==> 4 end, 300)
                       in unpair z as (cond-exp, const-exp) in +(cond-exp, const-exp)")
                 304 "cond-pair-3")
   
   ;; Pairs in allof
   (check-equal? (run "let m = pair(100, allof zero?(8) and not(greater?(3, 7)) end)
                       in unpair m as (num, bool) in if bool then num else *(num, 3)")
                 300 "pair-allof-1")
   (check-equal? (run "let q = pair(allof zero?(0) end, if not(equal?(3,3)) then 4 else 10)
                       in unpair q as (bool, num) in allof bool and greater?(num, 4) end")
                 #t "pair-allof-2")
   (check-equal? (run "let a = pair(100, pair(50, pair(70, 0))) in unpair a as 
                       (b, c) in unpair c as (d, e) in unpair e as (f, g) in
                       if allof not(zero? (b)) and equal?(d, 50) and 
                       less?(g, b) end then f else d")
                 70 "pair-allof-3")
   
   ;; Pairs in letrec
   (check-equal? (run "letrec f(x) = if zero?(x) then unpair pair(10, 30) as (a, b) in +(a, b)
                       else +(x, (f -(x, 1))) in (f 5)")
                 55 "letrec-pairs")
   (check-equal? (run "letrec g(z) = if equal?(z, 1) then unpair pair(z, 100) as (i, j) in *(i, j)
                       else *(z, (g -(z, 1))) in (g 5)")
                 12000 "letrec-pairs-2")
                 
   
   
   ))
   
                 
(run-tests let-tests 'verbose)

(run-tests proc-tests 'verbose)

(run-tests letrec-tests 'verbose)

(run-tests arith-tests 'verbose)

(run-tests comparison-tests 'verbose)

(run-tests cond-tests 'verbose)

(run-tests allof-tests 'verbose)

(run-tests pairs-test 'verbose)


;; (run-tests (append let-tests proc-tests letrec-tests) 'verbose