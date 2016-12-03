{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}

import Mutation (
    get, set, def, Mutable, Pointer, StateOp(..), Memory)

-- | Takes a number <n> and memory, and stores two new values in memory:
--   - the integer (n + 3) at location 100
--   - the boolean (n > 0) at location 500
--   Return the pointer to each stored value, and the new memory.
--   You may assume these locations are not already used by the memory.
pointerTest :: Integer -> Memory -> ((Pointer Integer, Pointer Bool), Memory)
pointerTest num mem = ((firstPtr, secondPtr), secondMem) where
							firstPair = (def mem 100 (num + 3))
							firstPtr = fst firstPair
							firstMem = snd firstPair
							secondPair = (def firstMem 500 (num > 0))
							secondPtr = fst secondPair
							secondMem = snd secondPair

swap :: Mutable a => Pointer a -> Pointer a -> StateOp ()
swap (P p1) (P p2) = undefined

swapCycle :: Mutable a => [Pointer a] -> StateOp ()
swapCycle ((P p):ps) = undefined
