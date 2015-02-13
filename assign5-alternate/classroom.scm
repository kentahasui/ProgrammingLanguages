#lang eopl
;; Kenta Hasui
;; CS235
;; Final Assignment 2014

;; LET language, classroom examples

(require "environments.scm")
(require "data-structures.scm")  ; for expval constructors
(require "lang.scm")             ; for scan&parse
(require "interp.scm")           ; for value-of-program

;; run : string -> val
(define run
  (lambda (string)
    (expval->scheme-val (value-of-program (scan&parse string)))))

(define exp1 "-(44,33)" ) 
(define exp2 "-(-(44,33),22)" ) 
(define exp3 "-(55, -(22,11))" )
(define exp4 "-(x,1)" ) 
(define exp5 "-(foo,1)" ) 
(define exp6 "if zero?(0) then 3 else 4" )
(define exp7 "if zero?(1) then 3 else 4" ) ;; not eval un-taken branch
(define exp8 "if zero?(-(11, 11)) then 3 else foo" )

(define exp9 "let x = 3 in x" ) 
(define exp10 "let x = 3 in -(x,1)" ) 
(define exp11 "let x = -(4,1) in -(x,1)" )
(define exp12 "let x = 3 in let y = 4 in -(x,y)" )
(define exp13 "let x = 3 in let x = 4 in x" ) 
(define exp14 "let x = 3 in let x = -(x,1) in x" )

(define exp15 " let x = 4
  in cons(x,
          cons(cons(-(x,1),
                    emptylist),
               emptylist))")
(define exp16 "cons(1, 2)")

(define exp17 "car(cons(1,2))")
(define exp18 "cdr(cons(1,2))")
(define exp19 "cdr(cons(1, emptylist))")
(define exp20 "null?(emptylist)")

