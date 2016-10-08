; Note the use of plai here; this is required for "test"
#lang plai
(abridged-test-output #t)

; This imports your file; do not change this line!
(require "database.rkt")

; Test helpers - use these instead of the built-in syntactic forms.
; DON'T export them from database.rkt!
(define (And x y) (and x y))
(define (Or x y) (or x y))
(define (If x y z) (if x y z))

; Sample tables - add more tables!
; Ideas: empty table; table with just 1 attribute; 0 attributes; duplicates
(define Person
  '(("Name" "Age" "LikesChocolate") 
    ("David" 20 #t) 
    ("Jen" 30 #t) 
    ("Paul" 100 #f)))

(define Teaching
  '(("Name" "Course")
    ("David" "CSC324")
    ("Paul" "CSC108")
    ("David" "CSC343")
    ))


#|
All tests go below. 
We have divided our tests into five sections:
- No WHERE/ORDER BY
- WHERE clause, but no ORDER BY
- ORDER BY clause, but no WHERE
- Both WHERE and ORDER BY
- Nested queries

Please respect this categorization when you add your tests,
and your TAs will appreciate it!
|#


; ---- SELECT/FROM tests ----
; Select all
(test (SELECT * FROM Person)
      '(("Name" "Age" "LikesChocolate") 
        ("David" 20 #t) 
        ("Jen" 30 #t) 
        ("Paul" 100 #f)))

; Reordering columns
(test (SELECT '("Age" "LikesChocolate" "Name") FROM Person)
      '(("Age" "LikesChocolate" "Name")
        (20 #t "David") 
        (30 #t "Jen") 
        (100 #f "Paul")))

; Select creates duplicates
(test (SELECT '("Name") FROM Teaching)
      '(("Name")
        ("David")
        ("Paul")
        ("David")))

; Select given a literal table
(test
 (SELECT '("A" "B")
   FROM '(("C" "A" "B" "D")
          (1 "Hi" 5 #t)
          (2 "Bye" 5 #f)
          (3 "Hi" 10 #t)))
 '(("A" "B")
   ("Hi" 5)
   ("Bye" 5)
   ("Hi" 10)))