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


> data WherePenguinsLive =
>     Galapagos
>   | Antarctica
>   | Australia
>   | SouthAfrica
>   | SouthAmerica
>   deriving (Eq, Show)

> data Penguin =
>   Peng WherePenguinsLive
>   deriving (Eq, Show)

> -- is it South Africa? If so, return True
> isSouthAfrica :: WherePenguinsLive -> Bool
> isSouthAfrica SouthAfrica = True
> isSouthAfrica _           = False

> gimmeWhereTheyLive :: Penguin
>                    -> WherePenguinsLive
> gimmeWhereTheyLive (Peng whereitlives) =
>   whereitlives

> humboldt = Peng SouthAmerica
> gentoo = Peng Antarctica
> macaroni = Peng Antarctica
> little = Peng Australia
> galapagos = Peng Galapagos

> galapagosPenguin :: Penguin -> Bool
> galapagosPenguin (Peng Galapagos) = True
> galapagosPenguin _ = False

> antarcticPenguin :: Penguin -> Bool
> antarcticPenguin (Peng Antarctica) = True
> antarcticPenguin _ = False


> antarcticOrGalapagos :: Penguin -> Bool
> antarcticOrGalapagos p =
>      galapagosPenguin p
>   || antarcticPenguin p


Exercises: Variety Pack
1. Given the following declarations

> k (x, y) = x
> k1 = k ((4-1), 10)
> k2 = k ("three", (1 + 2))
> k3 = k (3, True)

a) What is the type of k?
k :: (a, b) -> a

b) What is the type of k2? Is it the same type as k1 or k3?
k2 :: [Char]. It's not the same type as k1 or k3, which because of the monomorphism restriction are both Integer.

c) Of k1, k2, k3, which will return the number
3 as the result?
Both k1 and k3 will return the number 3.

2. Fill in the definition of the following function

> f :: (a, b, c)
>   -> (d, e, f)
>   -> ((a, d), (c, f))
> f (a, b, c) (d, e, f) =
>   ((a, d), (c, f))

Remember that tuples have the same syntax for their type constructors and their data constructors.


Exercises: Case Practice
We’re going to practice using case expressions by rewriting functions.

First, rewrite if-then-else expressions into case expressions.
1. The following should return x when x is greater than y.
functionC x y = if (x > y) then x else y

> functionC x y = case x > y of
>   True -> x
>   False -> y

2. The following will add 2 to even numbers and otherwise simply
return the input value.

ifEvenAdd2 n = if even n then (n+2) else n

> ifEvenAdd2 n = case even n of
>   True -> n + 2
>   False -> n

The next exercise doesn’t have all the cases covered. See if you
can fix it.

3. The following compares a value, x, to zero and returns an in- dicator for whether x is a positive number or negative number. What if x is 0? You may need to play with the compare function a bit to find what to do.

nums x = case compare x 0 of
  LT -> -1
  GT -> 1

> nums x = case compare x 0 of
>   LT -> -1
>   GT -> 1
>   EQ -> 1


Exercises: Artful Dodgy
Given the following definitions tell us what value results from further applications. When you’ve written down at least some of the answers and think you know what’s what, type the definitions into a file and load them in GHCi to test your answers.

> -- Types not provided,
> -- try filling them in yourself.

> dodgy :: Num a => a -> a -> a
> dodgy x y = x + y * 10

> oneIsOne :: Num a => a -> a
> oneIsOne = dodgy 1

> oneIsTwo :: Num a => a -> a
> oneIsTwo = (flip dodgy) 2

1. For example, given the expression dodgy 1 0, what do you think will happen if we evaluate it?

dodgy 1 0 = 1

2. dodgy 1 1 = 11
3. dodgy 2 2 = 22
4. dodgy 1 2 = 21
5. dodgy 2 1 = 12
6. oneIsOne 1 = 11
7. oneIsOne 2 = 21
8. oneIsTwo 1 = 21
9. oneIsTwo 2 = 22
10. oneIsOne 3 = 31
11. oneIsTwo 3 = 23

*MoreFuncPatterns> dodgy 1 1
11
*MoreFuncPatterns> dodgy 2 2
22
*MoreFuncPatterns> dodgy 1 2
21
*MoreFuncPatterns> dodgy 2 1
12
*MoreFuncPatterns> oneIsOne 1
11
*MoreFuncPatterns> oneIsOne 2
21
*MoreFuncPatterns> oneIsTwo 1
21
*MoreFuncPatterns> oneIsTwo 2
22
*MoreFuncPatterns> oneIsOne 3
31
*MoreFuncPatterns> oneIsTwo 3
23
*MoreFuncPatterns>


Exercises: Guard Duty
1. It is probably clear to you why you wouldn’t put an otherwise in your top-most guard, but try it with avgGrade anyway and see what happens. It’ll be more clear if you rewrite it as an otherwise match: | otherwise = 'F'. What happens now if you pass a 90 as an argument? 75? 60?


avgGrade :: (Fractional a, Ord a) => a -> Char

> avgGrade x
>   | otherwise = 'F'
>   | y >= 0.9  = 'A'
>   | y >= 0.8  = 'B'
>   | y >= 0.7  = 'C'
>   | y >= 0.59 = 'D'
>   | y < 0.59  = 'F'
>   where y = x / 100

Every case now just evaluates to 'F'.

2. What happens if you take avgGrade as it is written and reorder the guards? Does it still typecheck and work the same? Try moving | y >= 0.7 = 'C' and passing it the argument 90, which should be an ‘A.’ Does it return an ‘A’?

> avgGradeReordered x
>   | y >= 0.7  = 'C'
>   | y >= 0.9  = 'A'
>   | y >= 0.8  = 'B'
>   | y >= 0.59 = 'D'
>   | y < 0.59  = 'F'
>   where y = x / 100

If guards are reordered, the code behaves differently.

*MoreFuncPatterns> avgGradeReordered 90
'C'

3. The following function returns

> pal xs
>   | xs == reverse xs = True
>   | otherwise = False

a) xs written backwards when it’s True
b) True when xs is a palindrome
c) False when xs is a palindrome
d) False when xs is reversed

b) is correct.

4. pal can take any list. It's fully polymorphic in the list element type.

5. What is the type of the function pal?

> pal :: Eq a => [a] -> Bool

6. The following function returns

> numbers x
>   | x < 0  = -1
>   | x == 0 = 0
>   | x > 0  = 1

a) the value of its argument plus or minus 1
b) the negation of its argument
c) an indication of whether its argument is a positive or negative number or zero
d) binary machine language

c) is correct.

7. What types of arguments can numbers take?

numbers can take args that have instances of Eq and Ord.

8. What is the type of the function numbers?

numbers :: (Ord a, Num a, Num b) => a -> b



7.11 Chapter Exercises
Multiple choice
1. A polymorphic function
a) changes things into sheep when invoked
b) has multiple arguments
c) has a concrete type
d) may resolve to values of different types, depending on inputs

The correct answer is d).

2. Two functions named f and g have types Char -> String and String -> [String] respectively. The composed function g . f has the type
a) Char -> String
b) Char -> [String]
c) [[String]]
d) Char -> String -> [String]

b) is correct.

3. A function f has the type Ord a => a -> a -> Bool and we apply it to one numeric value. What is the type now?
a) Ord a => a -> Bool
b) Num -> Num -> Bool
c) Ord a => a -> a -> Integer
d) (Ord a, Num a) => a -> Bool

d) is the type now.

4. A function with the type (a -> b) -> c
a) requires values of three different types
b) is a higher-order function
c) must take a tuple as its first argument
d) has its parameters in alphabetical order

b) is true. The function it takes is of type (a -> b).

5. Given the following definition of f, what is the type of f True?
f :: a -> a
f x = x
a) f True :: Bool
b) f True :: String
c) f True :: Bool -> Bool
d) f True :: a

a) is correct.


1. The following function returns the tens digit of an integral argument.

> tensDigit :: Integral a => a -> a
> tensDigit x = d
>   where xLast = x `div` 10
>         d     = xLast `mod` 10

a) First, rewrite it using divMod.

> tensDigit' = snd . flip divMod 10 . fst . flip divMod 10

b) Does the divMod version have the same type as the original version?

If we include a type signature, it's the same. Otherwise the monomorphism restriction will ensure that it gets defaulted to the concrete type Integer -> Integer.

c) Next, let’s change it so that we’re getting the hundreds digit instead. You could start it like this (though that may not be the only possibility):
hunsD x = d2
    where d = undefined ...


> hunsD = flip mod 10 . flip div 100

2. Implement the function of the type a -> a -> Bool -> a once each using a case expression and once with a guard.
foldBool :: a -> a -> Bool -> a
foldBool =
  error
  "Error: Need to implement foldBool!"

The result is semantically similar to if-then-else expressions but syntactically quite different. Here is the pattern matching version to get you started:
foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

> foldBool :: a -> a -> Bool -> a
> foldBool a b bool = case bool of
>   True -> a
>   False -> b

> foldBool' :: a -> a -> Bool -> a
> foldBool' a b bool
>   | bool      = a
>   | otherwise = b

3. Fill in the definition. Note that the first argument to our function is also a function which can be applied to values. Your second argument is a tuple, which can be used for pattern matching:

> g :: (a -> b) -> (a, c) -> (b, c)
> g f (a, c) = (f a, c)

4. For this next exercise, you’ll experiment with writing pointfree versions of existing code. This involves some new information, so read the following explanation carefully.
Type classes are dispatched by type. Read is a type class like Show, but it is the dual or “opposite” of Show. In general, the Read type class isn’t something you should plan to use a lot, but this exercise is structured to teach you something about the interaction between type classes and types.
The function read in the Read type class has the type:

read :: Read a => String -> a
Notice a pattern?
read :: Read a => String -> a
show :: Show a => a -> String

Write the following code into a source file. Then load it and run it in GHCi to make sure you understand why the evaluation results in the answers you see.

(see arith4.hs)


5. Next, write a pointfree version of roundTrip. (n.b., This refers to the function definition, not to its application in main.)

(see arith4.hs)

6. We will continue to use the code in module Arith4 for this exercise as well.
When we apply show to a value such as (1 :: Int), the 𝑎 that implements Show is Int, so GHC will use the Int instance of the Show type class to stringify our Int of 1.
However, read expects a String argument in order to return an 𝑎. The String argument that is the first argument to read tells the function nothing about what type the de-stringified result should be. In the type signature roundTrip currently has, it knows because the type variables are the same, so the type that is the input to show has to be the same type as the output of read.
Your task now is to change the type of roundTrip in Arith4 to (Show a, Read b) => a -> b. How might we tell GHC which instance of Read to dispatch against the String now? Make the expression print (roundTrip 4) work. You will only need the `has the` type syntax of :: and parentheses for scoping.
