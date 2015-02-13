#lang eopl

  ;; grammar for the LETREC language
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)

      (expression (number) const-exp)
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      
      (expression (number) const-exp)
      (expression
        ("+" "(" expression "," expression ")")
        add-exp)
      
      (expression (number) const-exp)
      (expression
        ("*" "(" expression "," expression ")")
        mult-exp)
      
      (expression (number) const-exp)
      (expression
        ("/" "(" expression "," expression ")")
        div-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("equal?" "(" expression "," expression ")")
       equal?-exp)
      
      (expression
       ("less?" "(" expression "," expression ")")
       less?-exp)
      
      (expression
       ("greater?" "(" expression "," expression ")")
       greater?-exp)
      
      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression 
       ("cond" (arbno expression "==>" expression) "end")
       cond-exp)
      
      (expression
       ("allof" expression (arbno "and" expression) "end")
       allof-exp)
      
      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)   

      (expression
       ("proc" "(" identifier ")" expression)
       proc-exp)

      (expression
       ("(" expression expression ")")
       call-exp)

      (expression
        ("letrec"
          identifier "(" identifier ")" "=" expression
           "in" expression)
        letrec-exp)
      
      (expression 
       ("pair" "(" expression "," expression ")")
       pair-exp)
      
      (expression
       ("unpair" identifier "as" "(" identifier "," identifier ")" "in" expression)
       unpair-exp)
      
      ))
  
  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  

