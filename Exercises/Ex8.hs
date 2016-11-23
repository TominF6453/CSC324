{- Exercise 8, due (Nov 26, 11:50pm)

General exercise instructions:
- Exercises must be done *individually*.
- You may not import any Haskell libraries, unless explicitly told to.
- You may write helper functions freely; in fact, you are encouraged
  to do so to keep your code easy to understand.
- Your grade will be determined by our automated testing.
  You can find some sample tests on the course webpage.
- Submit early and often! MarkUs is rather slow when many people
  submit at once. It is your responsibility to make sure your work is
  submitted on time.
- No late submissions will be accepted!

This exercise is mainly a chance to practice writing code in Haskell,
although we'll take advantage of lazy evaluation to create some nice
infinite data structures. :)
-}

-- This line creates a module to allow exporting of functions.
-- DON'T CHANGE IT!
module Ex8 (geoSeq, monadIf, monadIf2) where

-- Question 1: Infinite sequences
-- This question is meant to give you a bit of practice with
-- infinite data structures in Haskell.
-- Define a function 'geoSeq', which takes two numbers a and r,
-- and returns the infinite list [a, ar, ar^2, ar^3, ar^3, ...]
geoSeq :: Num a => a -> a -> [a]
geoSeq x y = x:(geoSeq (x * y) y)

-- Question 2: Monads
-- This question is meant to give you a bit of experience working with Monads.
-- Remember that the Monad type class contains three functions (>>), (>>=),
-- and return. Use these to implement monadIf.
-- In other words, the return value should be the first argument chained
-- (using (>>=)) to the second if the first argument "returns" True,
-- or the first argument chained to the third if the first returns False.

-- Note: you'll be tempted to look up documentation on Monads,
-- for other functions which can help solve this problem. Don't! You can do
-- this using only the three base Monad functions.
monadIf :: Monad m => m Bool -> m a -> m a -> m a
monadIf x y z = x >>= \t -> if t then y else z

-- Question 3: Now with Either
-- First, read through the few paragraphs on Either in LYAH:
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses
-- (about halfway down the page, do a search for "Either a b").
-- For your reference - leave commented out.
-- data Either a b = Left a | Right b

-- Then implement monadIf2, which enables you to pass two monadic values
-- of different types, and return the correct one wrapped in an 'Either'.
-- The behaviour should be essentially the same as monadIf, except
-- for the 'Either'.

-- HINT: use return :: Monad m => a -> m a. This allows you to inject
-- a value into a monadic context.
monadIf2 :: Monad m => m Bool -> m a -> m b -> m (Either a b)
monadIf2 x y z = x >>= \t -> if t then y >>= left else z >>= right

left :: Monad m => a -> m (Either a _)
left x = return (Left x)

right :: Monad m => a -> m (Either _ a)
right x = return (Right x)
