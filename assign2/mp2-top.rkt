#lang eopl

;; CMPU-235 Fall 2014
;; Assignment 2
;; mp2-top.rkt
;; Kenta Hasui

(require rackunit
         rackunit/text-ui
         "mp2-soln.rkt")

(define mp2-tests
  (test-suite
   "tests for value-of-aexp-string"
   (check-equal? (value-of-aexp-string "33") 33 "constant test")
   (check-equal? (value-of-aexp-string "/(200, 10)") 20 "divide-1")
   (check-equal? (value-of-aexp-string "/(5, 2)") 5/2 "divide-2")
   (check-equal? (value-of-aexp-string "- (30,   50)") -20 "subt-1")
   ;(check-equal? (value-of-aexp-string "+(33)") 33 "sum-1")
   (check-equal? (value-of-aexp-string "+(33,44)") 77 "sum-2")
   (check-equal? (value-of-aexp-string "*(22,2)") 44 "arith-product-1")
   (check-equal? (value-of-aexp-string "*(30, 10)") 300 "product-2")
   (check-equal? (value-of-aexp-string "*(100, +(20, -(10, /(10, 2))))") 
                 2500
                 "divide-subt-add-mult")
   ;(check-equal? (value-of-aexp-string "-(5,2,3)") 0 "arith-diff-3")
   ))

(run-tests mp2-tests)