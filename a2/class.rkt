#lang racket
#| Assignment 2 - Classes

This file contains your work for Questions 1 and 2, extending the basic
class macro to include support for traits and some basic introspection.
|#
(provide class-meta class-trait)

; QUESTION 1 (metaprogramming).
(define-syntax class-meta
  (syntax-rules ()
    [(class-meta <Class> (<attr> ...)
       [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [(equal? msg "_attributes")
                (sort (list (list (id->string <attr>) <attr> ) ...)
                      string<?
                      #:key (λ(lst) (first lst)))]
               [(equal? msg "_methods")
                (sort (list (list (id->string <method>) (λ(<param> ...) <body>)) ...)
                      string<?
                      #:key (λ(lst) (first lst)))]
               [else "Unrecognized message!"]))
       )]))

; QUESTION 2 (traits).
(define-syntax class-trait
  (syntax-rules (with)
    [(class-trait <Class> (<attr> ...) (with)
                  [(<method> <param> ...) <body>] ...)
     (class-meta <Class> (<attr> ...)
                 [(<method> <param> ...) <body>] ...)]
    [(class-trait <Class> (<attr> ...) (with <trait> <next-traits> ...)
                 [(<method> <param> ...) <body>] ...)
     (let ([temp-class (class-trait <Class> (<attr> ...) (with <next-traits> ...)
                                    [(<method> <param> ...) <body>] ...)])
       (λ(msg)(cond [(not (equal? (<trait> msg) (temp-class msg))) ; if the message does match with the trait
                     (<trait msg>)] ; use the trait's intepretation of the message
                    [else (temp-class msg)] ; give the message to temp-class to try again
                    )))]))

; -----------------------------------------------------------------------------
; Class macro. This section is just for your reference.
; -----------------------------------------------------------------------------
(define-syntax class
  (syntax-rules ()
    [(class <Class> (<attr> ...)
       [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"]))
       )]))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))
