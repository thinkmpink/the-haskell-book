Chapter 5. Types

> module Types where

Exercises: Type Matching
Below you’ll find a list of several standard functions we’ve talked about previously. Under that is a list of their type signatures. Match the function to its type signature. Try to do it without peeking at the type signatures (either in the text or in GHCi) and then check your work. You may find it easier to start from the types and work out what you think a function of that type would do.

1. Functions:
a) not
b) length
c) concat
d) head
e) (<)

2. Type signatures:
a) _ :: [a] -> a
b) _ :: [[a]] -> [a]
c) _ :: Bool -> Bool
d) _ :: [a] -> Int
e) _ :: Ord a => a -> a -> Bool

a) -> c) not :: Bool -> Bool
b) -> d) length :: [a] -> Int
c) -> b) concat :: [[a]] -> [a]
d) -> a) head :: [a] -> a
e) -> e) (<) :: Ord a => a -> a -> Bool



Exercises: Type Arguments
Given a function and its type, tell us what type results from applying some or all of the arguments.
You can check your work in the REPL like this (using the first question as an example):
Prelude> f :: a -> a -> a -> a; f = undefined
Prelude> x :: Char; x = undefined
Prelude> :t f x
It turns out that you can check the types of things that aren’t implemented yet, so long as you give GHCi an undefined to bind the signature to.
1. If the type of f is a -> a -> a -> a, and the type of 𝑥 is Char then the type of f x is
a) Char -> Char -> Char
b) x -> x -> x -> x
c) a -> a -> a
d) a -> a -> a -> Char

a) f x :: Char -> Char -> Char


2. If the type of g is a -> b -> c -> b, then the type of g 0 'c' "woot" is
a) String
b) Char -> String
c) Int
d) Char

d) g 0 'c' "woot" :: Char


3. If the type of h is (Num a, Num b) => a -> b -> b, then the type of h 1.0 2 is:
a) Double
b) Integer
c) Integral b => b
d) Num b => b
Note that because the type variables 𝑎 and 𝑏 are different, the compiler must assume that the types could be different.

d) h 1.0 2 :: Num b => b, because 2 is only constrained (for now) to be a Num.


4. If the type of h is (Num a, Num b) => a -> b -> b, then the type of h 1 (5.5 :: Double) is
a) Integer
b) Fractional b => b
c) Double
d) Num b => b

c) h 1 (5.5 :: Double) :: Double


5. If the type of jackal is (Ord a, Eq b) => a -> b -> a, then the type of
     jackal "keyboard" "has the word jackal in it"
a) [Char]
b) Eq b => b
c) b -> [Char]
d) b
e) Eq b => b -> [Char]

a) jackal "keyboard" "has the word jackal in it" :: [Char]


6. If the type of jackal is (Ord a, Eq b) => a -> b -> a, then the type of
     jackal "keyboard"
a) b
b) Eq b => b
c) [Char]
d) b -> [Char]
e) Eq b => b -> [Char]

e) jackal "keyboard" :: Eq b => b -> [Char]


7. If the type of kessel is (Ord a, Num b) => a -> b -> a, then the type of
     kessel 1 2 is
a) Integer
b) Int
c) a
d) (Num a, Ord a) => a
e) Ord a => a
f) Num a => a

d) kessel 1 2 :: (Num a, Ord a) => a, because binding a to the value 1 will result in adding the constraint Num a to the existing constraint Ord a.


8. If the type of kessel is (Ord a, Num b) => a -> b -> a, then the type of
     kessel 1 (2 :: Integer) is
a) (Num a, Ord a) => a
b) Int
c) a
d) Num a => a
e) Ord a => a
f) Integer

a) kessel 1 (2 :: Integer) :: (Num a, Ord a) => a, because it doesn't matter what type b is bound to.


9. If the type of kessel is (Ord a, Num b) => a -> b -> a, then the type of
     kessel (1 :: Integer) 2 is
a) Num a => a
b) Ord a => a
c) Integer
d) (Num a, Ord a) => a
e) a

c) kessel (1 :: Integer) 2 :: Integer, because we bound a to Integer in the first argument to kessel



Exercises: Parametricity
All you can do with a parametrically polymorphic value is pass or not pass it to some other expression. Prove that to yourself with these small demonstrations.
1. Given the type a -> a, which is the type for id, attempt to make a function that terminates successfully that does something other than returning the same value. This is impossible, but you should try it anyway.

> id' :: a -> a
> id' a = id' a -- doesn't terminate to my knowledge

2. We can get a more comfortable appreciation of parametricity by looking at a -> a -> a. This hypothetical function a -> a -> a has two–and only two–implementations. Write both possible versions of a -> a -> a. After doing so, try to violate the constraints of parametrically polymorphic values we outlined above.

> binary1 :: a -> a -> a
> binary1 a1 a2 = a1

> binary2 :: a -> a -> a
> binary2 a1 a2 = a2

The following (and similar) won't work without additional constraints:
binary3 :: a -> a -> a
binary3 a1 a2 = a1 * a2


3. Implement a -> b -> b. How many implementations can it have? Does the behavior change when the types of 𝑎 and 𝑏 change?

> abb1 :: a -> b -> b
> abb1 _ b = b

This is the only implementation it has. And the behavior is the same for all types a and b.


Exercises: Apply Yourself
Look at these pairs of functions. One function is unapplied, so the compiler will infer a maximally polymorphic type. The second function has been applied to a value, so the inferred type signature may have become concrete, or at least less polymorphic. Figure out how the type would change and why, make a note of what you think the new inferred type would be and then check your work in GHCi.
1. -- Type signature of general function (++) :: [a] -> [a] -> [a]
-- How might that change when we apply -- it to the following value?
myConcat x = x ++ " yo"
2. -- General function
(*) :: Num a => a -> a -> a
     -- Applied to a value
myMult x = (x / 3) * 5
3. take :: Int -> [a] -> [a]
myTake x = take x "hey you"
4. (>) :: Ord a => a -> a -> Bool
myCom x = x > (length [1..10]) 5. (<) :: Ord a => a -> a -> Bool
myAlph x = x < 'z'
