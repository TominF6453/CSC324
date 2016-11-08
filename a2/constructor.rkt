#lang racket

; QUESTION 3 (fake constructor)
#|
WE NEED ONE-TWO PARAGRAPHS OF DOCSTRING HERE

Python example:
> (define (f r) (+ r 5))
> (class-constructor MyClass
    [(init (a b)
     (let* ([r (f a)]
            [this.x (f a)]
            [this.y (list (b 100 r))]
            [this.z "you are cool"])
|#
(define-syntax class-construct
  (syntax-rules ()

    ))