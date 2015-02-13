#lang eopl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CS235 - Programming Languages
;; Fall 2014
;; MP1: EOPL3: 1.14, 1.17-1.18, 1.20-1.23, 1.26-1.27.
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit 
         rackunit/text-ui)
;;(require "mp1-sols.scm")  
(require "assign1.rkt")

(define mp1-tests
  (test-suite
   "tests for MP1 exercises"
   
   ;; tests for down
  
   (check-equal? (down '(1 2 3)) '((1) (2) (3)) "down test 1")
   (check-equal? (down '((a) (fine) (idea))) 
                 '(((a)) ((fine)) ((idea))) 
                 "down test 2")
   (check-equal? (down '(a (more (complicated)) object)) 
                 '((a) ((more (complicated))) (object)) 
                 "down test 3")
   (check-equal? (down '()) '() "down test 4")
   (check-equal? (down '( hello (!) ((there) (my name is)) (((kenta))))) 
                 '( (hello) ((!)) ( ((there) (my name is))) ((((kenta)))))
                 "down test 5")
   (check-equal? (down '( () ($ ! (* (m))) 100)) 
                 '( (()) (($ ! (* (m)))) (100))
                 "down test 6")
   (check-equal? (down '())
                 '()
                 "down test 7")
   
;   ;; tests for swapper
   (check-equal? (swapper 'a 'd '(a b c d)) '(d b c a) "swapper test 1")
   (check-equal? (swapper 'a 'd '(a d () c d)) 
                 '(d a () c a) 
                 "swapper test 2")
   (check-equal? (swapper 'x 'y '((x) y (z (x)))) 
                 '((y) x (z (y))) 
                 "swapper test 3")
   (check-equal? (swapper 10 20 '()) '() "swapper test 4")
   (check-equal? (swapper 'b 'd '(x (d b) (b (c d (b b) d)) b))
                 '(x (b d) (d (c b (d d) b)) d)
                 "swapper test 5")
   (check-equal? (swapper '* '/ '(a / b (@ * $ (%)) b / a))
                 '(a * b (@ / $ (%)) b * a)
                 "swapper test 6")
   (check-equal? (swapper 'hello 'for '(for (is it me you're) looking hello))
                 '(hello (is it me you're) looking for)
                 "swapper test 7")
   (check-equal? (swapper 'a 'b '(c (d) e f gh))
                 '(c (d) e f gh)
                 "swapper test 8")
   (check-equal? (swapper 'a 'b '(a a a b b b))
                 '(b b b a a a)
                 "swapper test 9")
;   
;   ;; tests for count-occurrences
   (check-equal? (count-occurrences 'x '((f x) y (((x z) x)))) 
                 3
                 "count-occurrences test 1")
   (check-equal? (count-occurrences 'x '((f x) y (((x z) () x)))) 
                 3 
                 "count-occurrences test 2")
   (check-equal? (count-occurrences 'w '((f x) y (((x z) x)))) 
                 0
                 "count-occurrences test 3")
   (check-equal? (count-occurrences '* '()) 0 "count-occurrences test 4")
   (check-equal? (count-occurrences '@ '(@ (@ b) (@ (@ ^) @) (@ $ @ (@ (@ @ @)))))
                 11
                 "count-occurrences test 5")
   (check-equal? (count-occurrences '! '(hello ! (my ! (name) ! ) is (((!))bob)))
                 4
                 "count-occurrences test 6")
   
   (check-equal? (count-occurrences 'it '(it is hard (it is easy) (i t)))
                 2
                 "count-occurrences test 7")
   
;   ;; tests for product
   (check-equal? (product '(a b c) '(x y)) 
                 '((a x) (a y) (b x) (b y) (c x) (c y))
                 "product test 1")
   (check-equal? (product '(a) '(x y z)) 
                 '((a x) (a y) (a z))
                 "product test 2")
   (check-equal? (product '(a b c) '(x)) 
                 '((a x) (b x) (c x))
                 "product test 3")
   (check-equal? (product '($ &) '(! @))
                 '(($ !) ($ @) (& !) (& @))
                 "product test 4")
   (check-equal? (product '() '())
                 '()
                 "product test 5")
   (check-equal? (product '(w x y z) '())
                 '()
                 "product test 6")
   (check-equal? (product '() '(a b c d e f))
                 '()
                 "product test 7")
;   
;   ;; tests for filter-in
   (check-equal? (filter-in number? '(a 2 (1 3) b 7)) 
                 '(2 7) 
                 "filter-in test 1")
   (check-equal? (filter-in symbol? '(a (b c) 17 foo)) 
                 '(a foo) 
                 "filter-in test 2")
   (check-equal? (filter-in boolean? '(#f ((gaa $) (number? 2))))
                 '(#f)
                 "filter-in test 3")
   (check-equal? (filter-in char? (list #\b #\z 'boo 'h))
                 (list #\b #\z)
                 "filter-in test 4")
   (check-equal? (filter-in symbol? '(hello (there my) name 1 i s ((jim))))
                 '(hello name i s)
                 "filter-in test 5")
   (check-equal? (filter-in null? '(()))
                 '(())
                 "filter-in test 6")
   (check-equal? (filter-in even? '(2 4 3 ))
                 '(2 4))
   (check-equal? (filter-in number? '(a b c d e))
                '()
                "filter-in test 7")
   (check-equal? (filter-in number? '(1 2 3 4 5))
                 '(1 2 3 4 5)
                 "filter-in test 8")
   
   ;; tests for list-index
   (check-equal? (list-index number? '(a 2 (1 3) b 7)) 
                 1
                 "list-index test 1")
   (check-equal? (list-index symbol? '(a (b c) 17 foo)) 
                 0
                 "list-index test 2")
   (check-equal? (list-index symbol? '(1 2 (a b) 3)) 
                 #f
                 "list-index test 3")
   (check-equal? (list-index boolean? '(1 2 (a b) 3 (#f) #t)) 
                 5
                 "list-index test 4")
   (check-equal? (list-index char? '()) 
                 #f
                 "list-index test 5")
   (check-equal? (list-index char? (list '10 10 'c #\c 'hi '(#\d))) 
                 3
                 "list-index test 6")
;   
;   ;; tests for up
   (check-equal? (up '((1 2) (3 4))) 
                 '(1 2 3 4)
                 "up test 1")
   (check-equal? (up '((x (y)) z))
                 '(x (y) z)
                 "up test 2")
   (check-equal? (up '())
                 '()
                 "up test 3")
   (check-equal? (up (down '(a (b) (caa) (a (bdoo 4 @)))))
                 '(a (b) (caa) (a (bdoo 4 @)))
                 "up test 4")
   (check-equal? (down (up '((a) ((b c)) (((d))))))
                 '((a) ((b c)) (((d))))
                 "up test 5")
;   
;   ;; tests for flatten
   (check-equal? (flatten '(a b c)) '(a b c) "flatten test 1") 
   (check-equal? (flatten '((a) () (b ()) () (c))) '(a b c) "flatten test 2") 
   (check-equal? (flatten '((a b) c (((d)) e))) '(a b c d e) "flatten test 3") 
   (check-equal? (flatten '(a b (() (c)))) '(a b c) "flatten test 4")    
   (check-equal? (flatten '()) '() "flatten test 5") 
   (check-equal? (flatten '(hi () my name (is) () (slim sh (ady)) ))
                 '(hi my name is slim sh ady)
                 "flatten test 6") 
   (check-equal? (flatten '())
                 '()
                 "flatten test 7")
))

(run-tests mp1-tests)