#lang eopl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CS235 - Programming Languages
;; Fall 2014
;; MP1: EOPL3: 1.14, 1.17-1.18, 1.20-1.23, 1.26-1.27.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide down
         swapper
         count-occurrences
         product
         filter-in
         list-index
         up
         flatten
         )


#|
Exercise 1.17 [*] (down lst) wraps parentheses around each top-level element
of lst.
> (down ’(1 2 3)) 
((1) (2) (3)) 
> (down ’((a) (fine) (idea))) 
(((a)) ((fine)) ((idea))) 
> (down ’(a (more (complicated)) object)) 
((a) ((more (complicated))) (object))
|#

;; ---------------------------------------------------------------------
;; down: list -> list
;; usage: (down lst)
;; produces: An list like lst, but with parentheses around 
;;            each top-level element of lst. 
;; ---------------------------------------------------------------------

(define down
  (lambda (lst)
    (if (null? lst) ;; lst is empty = base case
        '()         ;; return the empty list
        (cons 
         (cons (car lst) '()) ;; Otherwise if list is not empty, cons the first element
                              ;; with an empty list to create an extra set of parentheses
         (down (cdr lst)))))) ;; Then call down recursively on the rest of the list. 

#|
Exercise 1.18 [*] (swapper s1 s2 slist) returns a list the same as slist, 
but with all occurrences of s1 replaced by s2 and all occurrences of s2 
replaced by s1.
> (swapper 'a 'd '(a b c d)) 
(d b c a) 
> (swapper 'a 'd '(a d () c d)) 
(d a () c a) 
> (swapper 'x 'y '((x) y (z (x)))) 
((y) x (z (y)))
|#

;; ---------------------------------------------------------------------
;; swapper: symbol * symbol * slist -> slist
;; usage: (swapper s1 s2 slist)
;; produces: An s-list like slist, but with each instance of s1 replaced 
;;          by s2 and each instance of s2 replaced by s1
;; ---------------------------------------------------------------------

(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist) ;; An s-list is either null
        '()
        (cons  ;; Or it is made up of an s-exp and and s-list. 
         ;; We call the swapping procedure on the s-expression at the car of slist
         (swap-in-sexp s1 s2 (car slist)) 
         ;; And we call swapper on the cdr of slist, which is an s-list.
         (swapper s1 s2 (cdr slist))))))

;; ------------------------------------------------------------------
;; swap-in-sexp: sym * sym * s-exp -> s-exp
;; usage: (swap-in-sexp s1 s2 sexp)
;; produces: a copy of sexp with all occurences of s1 replaced by s2, 
;;            and all occurences of s2 replaced by s1. 
;; ------------------------------------------------------------------

;; An auxiliary procedure for swapper for s-expressions.
;; Necessary for nested lists
(define swap-in-sexp        
  (lambda (s1 s2 sexp)
    ;; If the s-exp is a symbol, we can compare it to the values s1 and s2. 
    (if (symbol? sexp)
        (cond
          ;; If s-exp is equal to s1 we replace it with s2. 
          [(eqv? s1 sexp) s2]
          ;; If s-exp is equal to s2 we replace it with s1. 
          [(eqv? s2 sexp) s1]
          ;; Otherwise we leave the s-exp as is. 
          [else sexp])
        ;; If the s-exp is an s-list, we must treat it as an s-list
        ;; and call swapper on it.
        (swapper s1 s2 sexp))))

#| 
Exercise 1.20 [*] (count-occurrences s slist) returns the number of 
occurrences of s in slist.
> (count-occurrences 'x '((f x) y (((x z) x)))) 
3 
> (count-occurrences 'x '((f x) y (((x z) () x)))) 
3 
> (count-occurrences 'w '((f x) y (((x z) x)))) 
0
|#

;; your solution here

;; ------------------------------------------------------------------
;; count-occurrences: sym * s-list -> non-negative integer
;; usage: (count-occurrences s slist)
;; produces: the number of times s is found in slist.
;; ------------------------------------------------------------------


(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        ;; If the list is empty return 0, terminate recursion
        0
        ;; Otherwise an s-list is made up of an s-exp and an s-list
        (+ 
         ;; Count the number of times s appears in the s-exp
         (count-sexp s (car slist))
         ;; Add it to the number of times s appears in the remaining s-list
         (count-occurrences s (cdr slist))))))


;; ------------------------------------------------------------------
;; count-sexp: sym * s-expression -> non-negative integer
;; usage: (count-sexp s sexp)
;; produces: the number of times s is found in sexp. 
;;           auxiliary procedure for count-occurrences. 
;; ------------------------------------------------------------------

(define count-sexp
  (lambda (s sexp)
    (if (symbol? sexp)
        ;; If sexp is indeed a symbol, compare it to s. 
        ;; If it sexp = s, return 1. Otherwise return 0. 
        (if (eqv? s sexp) 1 0)
        ;; Otherwise if sexp is a list, call count-occurrences. 
        (count-occurrences s sexp))))

#|
Exercise 1.21 [**] (product sos1 sos2), where sos1 and sos2 are each a 
list of symbols without repetitions, returns a list of 2-lists that 
represents the Cartesian product of sos1 and sos2. The 2-lists may appear 
in any order.
> (product '(a b c) '(x y)) 
((a x) (a y) (b x) (b y) (c x) (c y))
|#

;; ------------------------------------------------------------------
;; product: list-of-symbols * list-of-symbols -> list of 2-lists
;; usage: (product sos1 sos2)
;; produces: a list of 2-lists that represents the Cartesian product of sos1
;;           and sos2. 
;; ------------------------------------------------------------------

(define product
  (lambda (sos1 sos2)
    (if (null? sos1) 
        ;; If sos1 is null, end the recursion.
        '()
        ;; Otherwise, call product-aux on the car of sos1
        ;; and recursively call product. 
        (append
         (product-aux (car sos1) sos2)
         (product (cdr sos1) sos2)))))

;; ------------------------------------------------------------------
;; product-aux: symbol * list-of-symbols -> list of 2-lists
;; usage: (product-aux sym sos2)
;; produces: a list of 2-lists, with each 2-list consisting of: 
;;           (sym sym2), where sym2 is a unique element in sos2.
;;           This procedure is used as a helper for the product procedure.
;; ------------------------------------------------------------------
(define product-aux
  (lambda (sym1 sos2)
    (if (null? sos2)
        '()
        ;; Create a list of 2-element lists
        (cons
         (list sym1 (car sos2))
         (product-aux sym1 (cdr sos2))))))

#|
Exercise 1.22 [**] (filter-in pred lst) returns the list of those elements 
in lst that satisfy the predicate pred.
> (filter-in number? '(a 2 (1 3) b 7)) (2 7) 
> (filter-in symbol? '(a (b c) 17 foo)) (a foo)
|#

;; ------------------------------------------------------------------
;; filter-in: predicate * list -> list
;; usage: (filter-in pred lst)
;; produces: the list of elements in lst that satisfy the predicate pred. 
;; ------------------------------------------------------------------

(define filter-in
  (lambda (pred lst)
    ;; If the list is empty return empty list.
    ;; Terminate recursion. 
    (if (null? lst)
        '()
        ;; Otherwise if the list is non-empty, 
        (if (pred (car lst))
            ;; If the first element of lst satisfies pred, 
            ;; cons it to the new list
            (cons (car lst) (filter-in pred (cdr lst)))
            ;; Otherwise do not cons the element. 
            ;; Simply recursively call filter-in on the rest of the list. 
            (filter-in pred (cdr lst))))))


#|
Exercise 1.23 [	] (list-index pred lst) returns the 0-based position of 
the first element of lst that satisfies the predicate pred. If no element 
of lst satisfies the predicate, then list-index returns #f.
> (list-index number? '(a 2 (1 3) b 7)) 1 
> (list-index symbol? '(a (b c) 17 foo)) 0
> (list-index symbol? '(1 2 (a b) 3)) #f
|#

;; ------------------------------------------------------------------
;; get-index: predicate * list -> number
;; usage: (list-index pred lst)
;; produces: the 0-based index of the first element of lst that satisfies
;;           pred. If none do, list-index returns the length of the list. 
;; ------------------------------------------------------------------

(define get-index
  (lambda (pred lst)
    (if (null? lst)
        0
        (if (pred (car lst))
            ;; If the car of lst satisfies pred, return the current index
            ;; and terminate the recursion.
            0
            ;; Otherwise add 1 to the index and check the next element of 
            ;; the list. 
            (+ 1 (get-index pred (cdr lst)))))))

;; ------------------------------------------------------------------
;; list-index: predicate * list -> number or #f
;; usage: (list-index pred lst)
;; produces: the 0-based index of the first element of lst that satisfies
;;           pred. If none do, list-index returns #f. 
;; ------------------------------------------------------------------

(define list-index
  (lambda (pred lst)
    ;; If the index is equal to the length of lst, 
    ;; we know that none of the elements of lst satisfy predl
    ;; Thus we return false. 
    (if (= (length lst)
           (get-index pred lst))
        #f
        ;; Otherwise we return the index. 
        (get-index pred lst))))

#|
Exercise 1.26 [**] (up lst) removes a pair of parentheses from each 
top-level element of list. If a top-level element is not a list, it is 
included in the result, as is. The value of (up (down lst)) is equivalent
to lst, but (down (up lst)) is not necessarily lst. (See exercise 1.17.)
> (up '((1 2) (3 4))) 
(1 2 3 4) 
> (up '((x (y)) z))
(x (y) z)
|#

;; ------------------------------------------------------------------
;; up: list -> list
;; usage: (up lst)
;; produces: Removes a pair of parentheses form each top-level element of list.
;;           If a top-level element is not a list, it is included in the result
;;           as-is. 
;; ------------------------------------------------------------------

(define up
  (lambda (lst)
    (if (null? lst) 
        '()
        (if (pair? (car lst))
            ;; If the first element of lst is a list,
            ;; append it to the rest of the list 
            ;; (this basically removes the outermost parentheses. 
            (append (car lst)
                    (up (cdr lst)))
            ;; Otherwise leave the first element as is. 
            (cons (car lst) (up (cdr lst)))))))

#|
Exercise1.27 [**] (flatten slist) returns a list of the symbols contained 
in slist in the order in which they occur when slist is printed. 
Intuitively, flatten removes all the inner parentheses from its argument.
> (flatten '(a b c)) (a b c) 
> (flatten '((a) () (b ()) () (c))) (a b c) 
> (flatten '((a b) c (((d)) e))) (a b c d e) 
> (flatten '(a b (() (c)))) (a b c)
|#

;; ------------------------------------------------------------------
;; flatten: s-list -> list
;; usage: (flatten slst)
;; produces: A list of the symbols in slist, with the order preserved.
;;           All inner parentheses are removed, as well as empty lists.
;; ------------------------------------------------------------------

(define flatten
  (lambda (slst)
    (cond
      ;; If the length of lst is 0, 
      ;; we reached the end so we stop the recursion.
      [(= (length slst) 0)
       '()]
      ;; If we haven't reached the end of lst and we find empty parens, 
      ;; we leave the parens out of the new list.
      [(eqv? '() (car slst))
       (flatten (cdr slst))]
      ;; If the car of lst is itself a list, we recursively call flatten 
      ;; on the car of lst. We then append the result to the recursive call 
      ;; of flatten on the cdr of list. This essentially removes the 
      ;; extra parentheses.
      [(pair? (car slst))
       (append (flatten (car slst))
               (flatten (cdr slst)))]
      ;; If the car of lst is not a list, we keep the car and 
      ;; recursively flatten the rest of lst. 
      [else
       (cons (car slst) (flatten (cdr slst)))])))
