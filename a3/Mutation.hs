{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
	Memory, Pointer(..), Value(..),
	Mutable, get, set, def,
	StateOp(..),
	(>>>), (>~>), returnVal,
	alloc, free)
	where

import AList (AList, lookupA, insertA, updateA, containsA)

-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
			 BoolVal Bool
			 deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer deriving Show

-- Type class representing a type which can be stored in "Memory".
class Mutable a where
	-- Look up a value in memory referred to by a pointer.
	get :: Memory -> Pointer a -> a

	-- Change a value in memory referred to by a pointer.
	-- Return the new memory after the update.
	set :: Memory -> Pointer a -> a -> Memory

	-- Create a new memory location storing a value, returning a new pointer
	-- and the new memory with the new value.
	-- Raise an error if the input Integer is already storing a value.
	def :: Memory -> Integer -> a -> (Pointer a, Memory)

extractInt :: Value -> Integer
extractInt (IntVal x) = x

extractBool :: Value -> Bool
extractBool (BoolVal b) = b

instance Mutable Integer where

	get mem (P p) = if (containsA mem p) then
						extractInt (lookupA mem p)
					else
						error "Doesn't exist"

	set mem (P p) val = if (containsA mem p) then
							updateA mem (p, (IntVal val))
						else
							error "Doesn't exist"

	def mem int val = if (containsA mem int) then
						  error "Already exists"
					  else
						  (P int, insertA mem (int, (IntVal val)))

instance Mutable Bool where

	get mem (P p) =	if (containsA mem p) then
						extractBool (lookupA mem p)
					else
						error "Doesn't exist"

	set mem (P p) val = if (containsA mem p) then
							updateA mem (p, (BoolVal val))
						else
							error "Doesn't exist"

	def mem int val = if (containsA mem int) then
						  error "Already exists"
					  else
						  (P int, insertA mem (int, (BoolVal val)))


-- StateOp declarations and such
data StateOp a = StateOp (Memory -> (a, Memory))

runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem

-- StateOp chaining declarations
-- "return"
returnVal :: a -> StateOp a
returnVal val = (StateOp val)

-- "then"
(>>>) :: StateOp a -> StateOp b -> StateOp b
op1 >>> op2 = \s ->
	let (_, s1) = op1 s
	in op2 s1

-- "bind"
(>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
(f >~> g) s =
	let (x, s1) = f s
		newStateOp = g x
	in newStateOp s1

-- | Memory allocation functions using StateOp
-- Allocating memory
alloc :: Mutable a => a -> StateOp (Pointer a)
alloc = undefined

-- Deallocating memory
free :: Mutable a => Pointer a -> StateOp ()
free = undefined
