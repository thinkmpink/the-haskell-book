Ch 7 -- More Functional Patterns

{-# LANGUAGE NoMonomorphismRestriction #-}

> module MoreFuncPatterns where

Exercises: Grab Bag
Note the following exercises are from source code files, not written for use directly in the REPL. Of course, you can change them to test directly in the REPL if you prefer.

1. Which (two or more) of the following are equivalent?
a) mTh x y z = x * y * z
b) mTh x y = \z -> x * y * z
c) mTh x = \y -> \z -> x * y * z
d) mTh = \x -> \y -> \z -> x * y * z

> mTh x y z = x * y * z

a), b), and c) are equivalent. If we enable the NoMonomorphismRestriction extension or add a type signature, then d) is equivalent to them too, otherwise its type is forced to be Integer -> Integer -> Integer -> Integer.

2. The type of mTh (above) is Num a => a -> a -> a -> a. Which is the type of mTh 3?
a) Integer -> Integer -> Integer
b) Num a => a -> a -> a -> a
c) Num a => a -> a
d) Num a => a -> a -> a

Since 3 is a polymorphic literal, mTh 3 :: Num a => a -> a -> a. d) is correct.

3. Next, we’ll practice writing anonymous lambda syntax.

Try to make it so it can still be loaded as a top-level definition by GHCi. This will make it easier to validate your answers.

a) Rewrite the f function in the where clause.

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f n = n + 1

> addOneIfOdd n = case odd n of
>   True -> f n
>   False -> n
>   where f = \n -> n + 1

b) Rewrite the following to use anonymous lambda syntax:

addFive x y = (if x > y then y else x) + 5

> addFive = \x -> \y -> (if x > y then y else x) + 5

c) Rewrite the following so that it doesn’t use anonymous lambda syntax:

mflip f = \x -> \y -> f y x

> mflip f x y = f y x
