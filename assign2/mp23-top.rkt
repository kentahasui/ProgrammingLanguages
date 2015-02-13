
#lang eopl
(require rackunit
         rackunit/text-ui
         "mp23-soln.rkt")

(define mp2-tests
  (test-suite
   "tests for value-of-aexp-string"
   (check-equal? (value-of-aexp-string "33") 33 "constant test")
   (check-equal? (value-of-aexp-string "/(200, 10)") 20 "divide-1")
   (check-equal? (value-of-aexp-string "/(5, 2)") 5/2 "divide-2")
   (check-equal? (value-of-aexp-string "- (30,   50)") -20 "subt-1")
   (check-equal? (value-of-aexp-string "+(33)") 33 "sum-1")
   (check-equal? (value-of-aexp-string "+(33,44)") 77 "sum-2")
   (check-equal? (value-of-aexp-string "*(22,2)") 44 "arith-product-1")
   (check-equal? (value-of-aexp-string "*(30, 10)") 300 "product-2")
   (check-equal? (value-of-aexp-string "*(100, +(20, -(10, /(10, 2))))") 
                 2500
                 "divide-subt-add-mult")
   
   (check-equal? (value-of-aexp-string "-(5,2,3)") 0 "arith-diff-3")
   (check-equal? (value-of-aexp-string "/(10,2,5,1)") 1 "divide-3")
   (check-equal? (value-of-aexp-string "*(5,2,25)") 250 "product-3")
   (check-equal? (value-of-aexp-string "+(100, 90, 10, 80, 20)") 300 "sum-3")
   (check-equal? (value-of-aexp-string "-(33)") 33 "subt-2")
   (check-equal? (value-of-aexp-string "*(88)") 88 "product-4")
   (check-equal? (value-of-aexp-string"/(1034)") 1034 "divide-4")
   (check-equal? (value-of-aexp-string "*(10, 20, +(30, 20), 22)") 220000 "product-5")
   
   (check-equal? (value-of-aexp-string "+()") 0 "empty-1")
   (check-equal? (value-of-aexp-string "*()") 1 "empty-2")
   (check-equal? (value-of-aexp-string "*(30, 10, +(5,  5), +())") 0 "empty-3")
   (check-equal? (value-of-aexp-string "*(5, 10, *())") 50 "empty-4")
   ))

(run-tests mp2-tests)