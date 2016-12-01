{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
    Memory, Pointer
    )
    where

import AList (AList, lookupA, insertA, updateA, containsA)

-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer


-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
    get :: Memory -> Pointer a -> a
	get mem (P x) = if (containsA mem x) then
						lookupA mem x
                    else
						error "Doesn't exist"

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: Memory -> Pointer a -> a -> Memory
	set mem (P x) val | containsA mem x = updateA mem (x, val)
				      | otherwise 		= error "Doesn't exist"

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: Memory -> Integer -> a -> (Pointer a, Memory)
