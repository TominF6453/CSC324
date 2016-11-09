#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require "choice.rkt")

; Export functions for testing. Please don't change this line!
(provide subsets sudoku-4 fold-<)

; QUESTION 3
#|
(subsets lst)
  lst: a list

  A choice expression which yields a subset of 'lst'.
  Repeated calls to 'next' should yield *all* other subsets
  (i.e., 'next' returns "false." only when all subsets of 'lst'
  have been returned).

  The subsets can be yielded in any order; however, no subset
  can appear twice.

  Note that:
    - A subset isn't the same as a sublist. Items don't have to be consecutive.
    - A subset can be empty; the empty set is a subset of any list.
    - Order doesn't matter in subsets
  
  The following is an example of how 'subsets' is used.
  Note that your implementation might yield the subsets
  in a different order than the one shown here.

> (subsets '(1 2))
'()
> (next)
'(1)
> (next)
'(2)
> (next)
'(1 2)
> (next)
"false."
|#

(define (subsets lst)
  (let ([x (unchecked-subsets lst)])
    (apply -< (remove-duplicates x))))

#| HELPER FUNCTIONS
(append2all lst element)
  lst: a list (of lists)
  element: the element to append

  Adds the element to each list within list.

> (append2all '((1 2 3 4) (5 6 7)) 8)
'((8 1 2 3 4) (8 5 6 7))
|#
(define (append2all lst element)
  (map (λ(elem) (append (list element) elem))
       lst))

#|
(unchecked-subsets lst)
  lst: a list

  Gets all subsets of list without checking for duplications.

> (unchecked-subsets '(1 2 2))
'(() (2) (2) (2 2) (1) (1 2) (1 2) (1 2 2))
|#
(define (unchecked-subsets lst)
  (cond [(empty? lst) (list empty)]
        [else (let ([x (unchecked-subsets (rest lst))])
                (append x (append2all x (first lst))))]))

; QUESTION 4
#|
(sudoku-4 puzzle)
  puzzle: a nested list representing a 4-by-4 Sudoku puzzle

  A choice expression that represents possible solutions to the puzzle.
  Upon evaluation, just one solution is returned, but repeated calls
  to 'next' produces more possible solutions, if any.

  Hint: use the ?- function in your solution. Again, your main task
  is just to correctly express the constraints, and let the computer
  do the work.
|#
(define sudoku-4 (void))


; QUESTION 5
#|
(fold-< combine init expr)
  combine: a binary function
  init: an initial value
  expr: a choice expression

  Evaluate all choices in <expr> and combine them, one at a time, with the
  initial value, and return the result.

  Note that the order of <combine>'s parameters is the same as foldl:
    1) The value of the next choice
    2) The value of <init>
|#
(define-syntax fold-<
  (syntax-rules ()
    ))