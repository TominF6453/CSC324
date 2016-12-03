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

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: Memory -> Pointer a -> a -> Memory

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: Memory -> Integer -> a -> (Pointer a, Memory)


-- StateOp declarations and such
data StateOp a = StateOp (Memory -> (a, Memory))

runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem

-- StateOp chaining declarations
-- "return"
returnVal :: a -> StateOp a
returnVal = undefined

-- "then"
(>>>) :: StateOp a -> StateOp b -> StateOp b
(>>>) = undefined

-- "bind"
(>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
(>~>) = undefined

-- | Memory allocation functions using StateOp
-- Allocating memory
alloc :: Mutable a => a -> StateOp (Pointer a)
alloc = undefined

-- Deallocating memory
free :: Mutable a => Pointer a -> StateOp ()
free = undefined
