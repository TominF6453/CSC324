#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require racket/include)
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
  (let ([x (checked-subsets lst)])
    (feed-< x)))

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

#|
(checked-subsets lst)
  lst: a list

  Gets all subsets of list, checking and removing duplicates.

> (checked-subsets '(1 2 2))
'(() (2) (2 2) (1) (1 2) (1 2 2))
|#
(define (checked-subsets lst)
  (remove-duplicates (unchecked-subsets lst)))

#|
(feed-< lsts)
  lsts: a list of lists

  Feeds -< to a list of lists.
|#
(define (feed-< lsts)
  (cond [(empty? lsts) empty]
        [(equal? (length lsts) 1) (first lsts)]
        [else (-< (first lsts) (feed-< (rest lsts)))]))

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
(define (sudoku-4 puzzle)
  void
  )


#|
Gets row (indexed 0, 1, 2, 3, going down) from puzzle
|#
(define (get-row row puzzle)
  (list-ref puzzle row))

#|
Gets column (indexed 0, 1, 2, 3, going left to right) from puzzle
|#
(define (get-column column puzzle)
  (map (λ(row)(list-ref row column))
       puzzle))

#|
Gets the soduku square (indexed 0, 1, 2, 3, going left to right, up to down) from puzzle
|#
(define (get-square square puzzle)
  (let ([flat-lst (flatten puzzle)])
    (cond [(equal? square 0) (list (list-ref flat-lst 0)
                                   (list-ref flat-lst 1)
                                   (list-ref flat-lst 4)
                                   (list-ref flat-lst 5))]
          [(equal? square 1) (list (list-ref flat-lst 2)
                                   (list-ref flat-lst 3)
                                   (list-ref flat-lst 6)
                                   (list-ref flat-lst 7))]
          [(equal? square 2) (list (list-ref flat-lst 8)
                                   (list-ref flat-lst 9)
                                   (list-ref flat-lst 12)
                                   (list-ref flat-lst 13))]
          [(equal? square 3) (list (list-ref flat-lst 10)
                                   (list-ref flat-lst 11)
                                   (list-ref flat-lst 14)
                                   (list-ref flat-lst 15))])))
    
#|
Returns whether or not the given soduku group is valid
|#
(define (valid-group group)
  (if (false? (check-duplicates group)) #t #f))

#|
Returns whether or not every element in bool-lst is true.
|#
(define (and-all bool-lst)
  (cond [(empty? bool-lst) #t]
        [else (and (first bool-lst)
                   (and-all (rest bool-lst)))]))

#|
Returns whether or not the given sudoku puzzle is valid
|#
(define (valid-puzzle puzzle)
  (let* ([indices '(0 1 2 3)]
        [rows (map (λ(index)(get-row index puzzle))
                   indices)]
        [cols (map (λ(index)(get-column index puzzle))
                   indices)]
        [sqrs (map (λ(index)(get-square index puzzle))
                   indices)]
        [groups (append rows cols sqrs)]
        [group-validity (map (λ(grp)(valid-group grp))
                             groups)])
    (and-all group-validity)))
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