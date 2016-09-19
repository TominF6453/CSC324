#| Exercise 1 - Getting started with Racket (due Sept 24, 11:50pm on Markus)

General exercise instructions:
- Exercises must be done *individually*.
- You may not import any Racket libraries, unless explicitly told to.
- You may not use mutation or any iterative constructs (for/*).
- You may write helper functions freely; in fact, you are encouraged
  to do so to keep your code easy to understand.
- Your grade will be determined by our automated testing.
  You can find some sample tests on the course webpage.
- Submit early and often! MarkUs is rather slow when many people
  submit at once. It is your responsibility to make sure your work is
  submitted on time.
- No late submissions will be accepted!

Implement the three functions below to get some experience programming in Racket.
You may use either explicit recursion, or higher-order list functions.
(For extra practice, try both!)
|#
#lang racket

; This line exports the required functions. Don't change it!
(provide search-table search-table-2 max-sublist)

#|
(search-table table item)
  table: a list of lists
  item: some value

  Returns a list containing the lists in 'table' which contain 'item'.
  The lists must appear in the same order they appear in 'table'.

> (search-table '((1 2 3) (4 2) (3 3 4) (5 3 2 1)) 1)
'((1 2 3) (5 3 2 1))
> (search-table '((1 2 3) (4 2) (3 3 4) (5 3 2 1)) 10)
'()
|#
; Feel free to change this signature to use the shorthand for defining functions
; (define (search-table ...) (...))
(define (contains? item lst) ;Helper function, returns #f or #t whether or not item is
  (cond                      ; is lst.
    [(member item lst) #t]
    [else #f]))

(define (search-table table item)
  (cond
    [(null? table) empty]
    [else (let([end (search-table (rest table) item)])
            (cond
              [(contains? item (first table)) (list* (first table) end)]
              [else end]))]))

#|
(search-table-2 table item [pos])
  table: a list of lists
  item: any value
  pos: (*optional* parameter) an index in a list. Default value is 0. 
  
  Returns a list containing the lists in 'table' which have 'item' 
  at position 'pos'.
  Lists with length <= 'pos' should, of course, not be included.
  The lists must appear in the same order they appear in 'table'.

> (search-table-2 '((1 2 3) (4 2) (3 3 4) (5 3 2 1)) 1 3)
'((5 3 2 1))
> (search-table-2 '((1 2 3) (4 2) (3 3 4) (5 3 2 1)) 1)
'((1 2 3))

Hint: look up "declaring optional arguments in Racket". The syntax is
pretty straight-forward. Note that optional arguments must appear
*after* all the required ones.
|#
(define (containspos? item lst pos)
  (cond
    [(null? lst) #f] ;List is empty
    [(not (and (integer? pos) (>= pos 0))) #f] ;pos not valid integer
    [(> (+ pos 1) (length lst)) #f] ;pos out of index
    [else (equal? (list-ref lst pos) item)]))

(define (search-table-2 table item [pos 0])
  (cond
    [(null? table) empty]
    [else (let ([end (search-table-2 (rest table) item pos)])
            (cond
              [(containspos? item (first table) pos) (list* (first table) end)]
              [else end]))]))

#|
(max-sublist lst)
  lst: a list of numbers

  Returns the maximum sum of a sublist of numbers in 'lst'.
  A *sublist* of a list is a series of consecutive numbers in the list.

  Note that the *empty list* is a valid sublist of every list,
  and has a sum of 0.

  You may choose to use a brute-force O(n^3) algorithm for this question.
  However, for extra learning you're encouraged to try to find a O(n) algorithm.
  (Personally, I think the linear algorithm is actually easier to implement.)

> (max-sublist '(-1 10 -4 5 3 -100 6))
14  ; sum of '(10 -4 5 3)
> (max-sublist '(-4 -1 -2 -3))
0   ; sum of '()

Hint: you may find the "apply" function helpful.
|#
(define (frest lst) ; Returns lst without the last element
  (cond
    [(null? list) empty]
    [else (reverse (rest (reverse lst)))]))

(define (frestsublist lst) ; Returns all sublists obtained by removing the last elements from lst
  (cond
    [(null? lst) empty]
    [else (let ([end (frestsublist (frest lst))])
            (cond
              [(not (null? lst)) (list* lst end)]))]))
    
(define (getsublists lst) ; Returns all possible sublists of lst
  (cond
    [(null? lst) empty]
    [else (let ([end (getsublists (rest lst))])
            (cond
              [(not (null? lst)) (append (frestsublist lst) end)]))]))

(define (subsums lst) ; Returns a list of the sums of each list in lst
  (cond
    [(null? lst) empty]
    [else (let ([end (subsums (rest lst))])
            (cond
              [(not (null? lst)) (list* (apply + (first lst)) end)]))]))

(define (max-sublist lst)
  (let ([lstsublists (getsublists lst)])
    (cond
      [(null? lstsublists) 0]
      [else (max 0 (apply max (subsums lstsublists)))])))