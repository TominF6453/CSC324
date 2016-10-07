#| Assignment 1 - Racket Query Language  (due Oct 14, 11:50pm)

***Write the names, CDF accounts and student IDs for each of your group members below.***
Filip Tomin, tominfil, 1001329984
Brendan Neal, nealbre1, 1001160226
|#
#lang racket

; Function versions for common syntactic forms.
; *Use these in your queries instead of the syntactic forms!!!*
(define (And x y) (and x y))
(define (Or x y) (or x y))
(define (If x y z) (if x y z))
(define (tail lst) (rest lst))
(define (head lst) (first lst))

; Generic helper functions
#|
(contains item lst)
   item: an object
   lst: a list of objects the same type as item

   Returns a boolean value indicating whether or not item is in lst.

> (contains 1 '(1 2 3))
#t
> (contains 1 '())
#f
> (contains 1 '(2 3))
#f
> (contains 1 '(2))
#f
> (contains 1 '(1))
#t
|#
(define (contains item lst)
  (cond [(null? lst) #f]
        [else (or (equal? (car lst) item)
                  (contains item (cdr lst))
               )
        ]
  )
)

; TODO: After you have defined your macro(s), make sure you add each one
; to the provide statement.
(provide attributes
         tuples
         size)

; Part 0: Semantic aliases

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)

  Returns a list of the attributes in 'table', in the order they appear.
|#
(define (attributes table)
  ; First row of the table is the attributes, so just return first element
  (head table))

#|
(tuples table)
  table: a valid table

  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#
(define (tuples table)
  ; Everything beyond the first index is a tuple
  ; Just return the tail of the list
  (tail table))

#|
(size table)
  table: a valid table

  Returns the number of tuples in 'table'.
|#
(define (size table)
  ; First element is the attribute specifier,
  ; so it cannot be included as part of size
  (- (length table) 1))


; Part I "WHERE" helpers; you may or may not wish to implement these.

#|
(get-value attribute-template target-attribute tup): 
  - attribute-template: a list of attributes
  - target-attribute: string (representing an attribute)
  - tup: a tuple

  Returns the value of the tuple corresponding to that attribute.
|#
(define (get-value attribute-template target-attribute tup)
  (let ([att-index (index-of target-attribute attribute-template)])
    (list-ref tup att-index)))

#|
(get-value attribute-template target-attributes tup): 
  - attribute-template: a list of attributes
  - target-attributes: a subset of attributes in attribute-template
  - tup: a tuple

  Returns a list of values of the tuple (in order of appearance
  in target-attributes) corresponding to the targeted attributes.
  Returns the empty list if there are no targeted attributes.
|#
(define (get-values attribute-template target-attributes tup)
  (cond [(null? target-attributes) empty]
        [else (append (list (get-value
                             attribute-template
                             (head target-attributes)
                             tup))
                      (get-values
                       attribute-template
                       (tail target-attributes)
                       tup))]))



#|
(tups-satisfying f table)
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table

  Returns a new table containing only the tuples in 'table'
  that satisfy 'f'.
|#
(define (tups-satisfying f table)
  ; Want the head of the list to be the attributes of the table
  ; Want the tail of the list to be the satisying tuples
  (append (head table)
          (filter f (tuples table))))

#|
(index-of x lst)
  - x: some object
  - lst: a list of objects

  Returns the index of x if x is in lst. If x is not in lst, return -1.

> (index-of 1 '(1 2 3))
0
> (index-of 2 '(1 2 3))
1
> (index-of 4 '(1 2 3))
-1
|#
(define (index-of x lst)
  (cond [(equal? #f (member x lst)) -1]
        [else (cond [(equal? (head lst) x) 0]
                    [else (+ 1 (index-of x (tail lst)))])]))

#|
A function 'replace-attr' that takes:
  - x 
  - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.
|#
(define (replace-attr x attr-lst)
  (If (false? (member x attr-lst))
      (λ(tuple) x)
      (λ(tuple) (list-ref tuple (index-of x attr-lst)))))

; Cartesian Product Functions - Used for FROM clause (multiple tables)
#|
(cartesian-helper table1 table2)
  table1: list of lists [K1, K2, ..., Km]
  table2: list of lists [L1, L2, ..., Ln]

  Returns a list of the contatenation of all possible pairs of lists, in the 
  order [K1 + L1, K1 + L2, ..., K1 + Ln, K2 + L1, ..., K2 + Ln, ..., Km + Ln]

  If at least one of 'table1' and 'table2' is empty, their Cartesian product
  does not contain any lists.

> (cartesian-helper '((1 4) (2 10)) '((3 4 5) (2)))
'((1 4 3 4 5) (1 4 2) (2 10 3 4 5) (2 10 2))
|#
(define (cartesian-helper table1 table2)
  (cond
    [(null? table1) empty]
    [(null? table2) empty]
    [else
     (append-map (λ (lst1) (map (λ (lst2) (append lst1 lst2)) table2)) table1)]))

#|
(cartesian-product table1 table2)
  table1, table2: lists of lists in the table format specified by the assignment

  Returns a list of the following form:
      Head: concatenation of the attributes of table1 and table2, in that order
      Tail: cartesian product of the tuples of table1 and table2, in the form:
            (table1 data, table2 data)

> (cartesian-product '(("A" "B") (1 2) (3 4)) '(("C") (5)))
'(("A" "B" "C") (1 2 5) (3 4 5))
|#
(define (cartesian-product table1 table2)
  (list* (append (head table1) (head table2))
         (cartesian-helper (tuples table1)
                           (tuples table2))))

; Use the multi-cartesian on the list of tables in FROM - need to implement column renaming
#|
(multi-cartesian table-list)
  table-list: list of (lists of lists in the table format specified by the assignment)

  Returns a list of the following form:
      Head: concatenation of the attributes of all the tables in table-list,
            in order of appearance
      Tail: cartesian product of the tuples of all the tables in table-list.

> (multi-cartesian (list '(("A" "B") (1 2) (3 4)) '(("C") (5)) '(("D") (6))))
'(("A" "B" "C" "D") (1 2 5 6) (3 4 5 6))
|#
(define (multi-cartesian table-list)
  (cond [(equal? 1 (length table-list))(head table-list)]
        [else (cartesian-product
               (head table-list)
               (multi-cartesian (tail table-list)))
              ]
        )
  )

#|
(same-attribute-names table1 table2)
     table1, table2: lists of lists in the table format specified by the assignment

     Returns a list of attributes that appear in borth table1 and table2

> (same-attribute-names '(("A" "C") (1 2)) '(("A" "B" "C") (4 5 6)))
'("A" "C")
|#
(define (same-attribute-names table1 table2)
  (filter (lambda (x) contains x (attributes table2))
          (attributes table1)
          )
  )

#|
(rename-same-attributes table1 table2)
     table1, table2: lists of lists in the table format specified by the assignment

     Returns a 2-long list of lists in the following format:
         First element: list of renamed attributes for table1
         Second element: list of renamed attributes for table2
     Attributes that have the same name 

> (same-attribute-names '(("A" "C") (1 2)) '(("A" "B" "C") (4 5 6)))
'("A" "C")
|#

; Starter for Part 3; feel free to ignore!

; What should this macro do?
(define-syntax replace
  (syntax-rules ()
    ; The recursive step, when given a compound expression
    [(replace (expr ...) table)
     ; Change this!
     (void)]
    ; The base case, when given just an atom. This is easier!
    [(replace atom table)
     ; Change this!
     (void)]))
