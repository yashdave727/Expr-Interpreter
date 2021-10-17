#lang racket #| * CSC324H5 Fall 2021: Assignment 1 * |#
#|
Module:        a1
Description:   Assignment 1: Checking for Tail Recursion
Copyright: (c) University of Toronto Mississsauga
               CSC324 Principles of Programming Languages, Fall 2021
|#

; This specifies which functions this module exports. Don't change this!
(provide is-tail-recursive)

; Import the testing library
(module+ test
  (require rackunit))


#|
(is-tail-recursive def) -> boolean? 
  def: (and/or symbol? number?) 
    A function definition that follows the grammar for <def> in the handout.
    You may assume that the function defined in def is recursive.

  Returns whether the definition is tail recursive
|#
(define (is-tail-recursive def)
    (let* ([fname (first (second def))]
         [body  (third def)])
     (if (has-call fname body) (all-tail-position fname body) #t))) 
  



; You can write helper functions freely

#|
(all-tail-position fname expr) -> boolean?
   fname: symbol?
   expr: from the expr grammar

Returns whether all calls to the function in the expr is in tail position
|#
(define/match (all-tail-position fname expr)
  [(fname (list 'let* (cons (list id expr) xs) ex))
   (cond [(has-call fname expr) #f]
         [(not (equal? '() (filter (lambda (y) (equal? y #t)) (map (lambda (y) (has-call fname (second y))) xs)))) #f]
         [else (all-tail-position fname ex)])
   ] ; let*

  [(fname (cons 'if xs))
   (cond
     [(has-call fname (first xs)) #f]
     [(equal? '(#t #t) (filter (lambda (y) (equal? y #t)) (map (lambda (y) (all-tail-position fname y)) (rest xs)))) #t]
     [else #f]
     )
   ] ; if
  
  [(fname (cons x xs))
    (equal? '() (filter (lambda (z) (equal? z #t)) (map (lambda (y) (has-call fname y)) xs))) ; tail-recursion has to be in the last expr -> check tail recur in last expr
     
   ] ; function call
  
  [(fname expr) #t] ; id and literal

  )

#|
(has-call fname expr) -> boolean?
Returns whether the expr contains a call to the function
|#
(define/match (has-call fname expr)
  
     [(fname (list 'let* (cons (list id expr) xs) ex)) ; xs is list of (id expr) & ex is list of expressions
       
       (cond   ; xs = '([id e] [id e2])  y = '(e e2 )
         ;[(has-call fname id) #t]
         [(has-call fname ex) #t]
         [(has-call fname expr) #t]
         ;[(has-call fname expr) #t]
         [else (not (equal? '() (filter (lambda (z) (equal? z #t)) (map (lambda (y) (has-call fname (second y))) xs))))]
        ) ;[(has-call fname (list 'let* xs ex)) #t]
         ;[else #f])
       ] ; let* expr

  [(fname (cons 'if xs)) ; xs is a list of expressions here
       (not (equal? '() (filter (lambda (y) (equal? y #t)) (map (lambda (y) (has-call fname y)) xs))))
      
       ] ; if expr
  
      [(fname (cons x xs))
       (if (equal? x fname)
           #t
       (not (equal? '() (filter (lambda (y) (equal? y #t)) (map (lambda (y) (has-call fname y)) xs)))))
       ]
  ; function call
    
  
      

      [(fname expr) #f] ; id and literal
      )

(module+ test
; all-tail-positions test cases ----------------------------------

  ; Base case test
  (test-equal? "all-tail Base test literal" 
               (all-tail-position 'g 3)
               #t)

  (test-equal? "all-tail Base test id"
               (all-tail-position 'g 'g)
               #t)

  (test-equal? "all-tail Base test id_2"
               (all-tail-position 'g 'f)
               #t)

  ; Function call test
  (test-equal? "all-tail Fun call_1"
               (all-tail-position 'f '(f x))
               #t)

  (test-equal? "all-tail Fun call_2"
               (all-tail-position 'f '(h (g(f(g x))) (f x)))
               #f)

  (test-equal? "all-tail Fun call_3"
               (all-tail-position 'f '(f(g(h(g 3)))))
               #t)

  (test-equal? "all-tail Fun call_4"
               (all-tail-position 'f '(g f))
               #t)

  (test-equal? "all-tail Fun call_5"
               (all-tail-position 'f '(g (f x) a))             ; ???
               #f)

  ; If test
  (test-equal? "all-tail if_1"
               (all-tail-position 'f '(if f (f x) (f x)))
               #t)

  (test-equal? "all-tail if_2"
               (all-tail-position 'f '(if (f x) (f x) g))
               #f)

  (test-equal? "all-tail if_3"
               (all-tail-position 'f '(if (g(h(f a))) g h))
               #f)
  (test-equal? "all-tail if_4"
               (all-tail-position 'f '(if g (g a) (g (f a))))
               #f)
  (test-equal? "all-tail if_5"
               (all-tail-position 'f '(if (g x) x (g (f x) x)))
               #f)

  (test-equal? "all-tail if_6"
               (all-tail-position 'f '(if g (g (f a)) (g (f a))))
               #f)
  

  ; let* test
  (test-equal? "all-tail let_1"
               (all-tail-position 'f '(let* ([x f] [y (f a)]) (g a)))
               #f)
  (test-equal? "all-tail let_2"
               (all-tail-position 'f '(let* ([c f]) (f a)))
               #t)
  (test-equal? "all-tail let_3"
               (all-tail-position 'f '(let* (x (f a)) (f g)))
               #f)
  (test-equal? "all-tail let_4"
               (all-tail-position 'f '(let* ([a (f x)]) a))
               #f)

  ;------------------------------------------------------------------------
  



  
; Has-Call Test cases -----------------------
  
  ; Base case tests

  (test-equal? "Base case test for has call"
               (has-call 'f 'f)
               #f)

  (test-equal? "Simple test for has call"
               (has-call 'f '(f x))
               #t)

  (test-equal? "Simple test_2 for has-call"
               (has-call 'f 3)
               #f)

   (test-equal? "Simple test_3 for has-call"
               (has-call 'f 'g)
               #f)
  
  ; Function call test cases

  (test-equal? "Simple test for function call"
               (has-call 'f '(f(g x)))
               #t)

  (test-equal? "Simple test_2 for function call"
               (has-call 'f '(g(g x)))
               #f)

  (test-equal? "Simple test_3 for function call"
               (has-call 'f '(g f))
               #f)

  (test-equal? "Simple test_4 for function call"
               (has-call 'g '(f x))
               #f)

  ; If test cases

  (test-equal? "Simple test for if"
               (has-call 'f '(if 3 3 3))
               #f)

  (test-equal? "Test_2 for if"
               (has-call 'f '(if (f a) (g c) d))
               #t)

  (test-equal? "Test_2 for if"
               (has-call 'f '(if (g a) (g(f a)) d))
               #t)

  (test-equal? "Test_3 for if"
               (has-call 'f '(if (g f) x f))
               #f)

  (test-equal? "Test_4 for if"
               (has-call 'g '(if (f g) (g(g x)) f))
               #t)

  ; let* test cases
  (test-equal? "Test_1 let"
               (has-call 'g '(let* ([val1 (g x)]) f))
               #t
               )

  (test-equal? "Test_2 let"
               (has-call 'g '(let* ([g f] [g f]) g))
               #f
               )

  (test-equal? "Test_3 let"
               (has-call 'g '(let* ([g f] [g f]) (g f)))
               #t
               )

  (test-equal? "Test_4 let"
                (has-call 'g '(let* ([g f] [g f]) (f(g x))))
                #t
               )
  ;----------------------------------------------------------------
  
  ; We use rackunit's test-equal? to define some simple tests.
  (test-equal? "Simple test for tail recursion"        ; Test label
               (is-tail-recursive '(def (f x) (f x)))  ; Actual value
               #t)                                     ; Expected value
  (test-equal? "Recursive call in if expression conditional"
               (is-tail-recursive '(def (f x) (if (f x) x x)))
               #f)
  (test-equal? "Recursive call in let* definition"
               (is-tail-recursive '(def (f x) (let* ([a (f x)]) (g a))))
               #f)
  (test-equal? "Recursive call in let* body"
               (is-tail-recursive '(def (f x) (let* ([a (g x)]) (f a))))
               #t)

  ; TODO: Write more tests. Testing is an important part of programming,
  ; so ou and your partner must write your own tests. Do not share your
  ; tests with anyone else.
)

