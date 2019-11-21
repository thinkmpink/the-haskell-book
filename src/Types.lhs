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

myConcat x :: [Char] -> [Char]

The " yo" binds [a] to [Char], and the concatenation to the concrete value removes a degree of freedom (an argument).


2. -- General function
(*) :: Num a => a -> a -> a
     -- Applied to a value
myMult x = (x / 3) * 5

myMult :: Fractional a => a -> a

While (*) doesn't constrain its Num input, the type of (/) is Fractional a => a -> a -> a. Hence both input and output of myMult will be something Fractional.

3. take :: Int -> [a] -> [a]
myTake x = take x "hey you"

myTake :: Int -> [Char]

Binding to "hey you" removes an argument to take, and binds [a] from take's signature to [Char] in myTake's signature.

4. (>) :: Ord a => a -> a -> Bool
myCom x = x > (length [1..10])

myCom :: Int -> Bool

Since the output of length is an Int, the type variable a is bound to Int and the degree of freedom is removed.

5. (<) :: Ord a => a -> a -> Bool
myAlph x = x < 'z'

myAlph :: Char -> Bool

The 'z' value of the concrete type Char forces the type variable a to bind to Char, and we remove the redundant Ord constraint and argument.



Chapter Exercises

Multiple choice
1. A value of type [a] is
a) a list of alphabetic characters
b) a list of lists
c) a list whose elements are all of some type 𝑎
d) a list whose elements are all of different types

c)

2. A function of type [[a]] -> [a] could
a) take a list of strings as an argument b) transform a character into a string
c) transform a string into a list of strings
d) take two arguments

a)

3. A function of type [a] -> Int -> a
a) takes one argument
b) returns one element of type 𝑎 from a list
c) must return an Int value
d) is completely fictional

This function must return an element. In a sense this must be fictional, because there would be no way for this function to produce a value given an empty list. But Haskell allows partial functions like this one and head, so I'll say the answer is b).

4. A function of type (a, b) -> a
a) takes a list argument and returns a Char value
b) has zero arguments
c) takes a tuple argument and returns the first value
d) requires that 𝑎 and 𝑏 be of different types

Only c) is true.


Determine the type
For the following functions, determine the type of the specified value. We suggest you type them into a file and load the contents of the file in GHCi. In all likelihood, it initially will not have the polymorphic types you might expect due to the monomorphism restriction. That means that top-level declarations by default will have a concrete type if any can be determined. You can fix this by setting up your file like so:

(see DetermineTheType.hs)


Does it compile?
For each set of expressions, figure out which expression, if any, causes the compiler to squawk at you (n.b. we do not mean literal squawking) and why. Fix it if you can.
1. bigNum = (^) 5 $ 10
   wahoo = bigNum $ 10

This will not compile because bigNum is a fully applied value, and because GHC's monomorphism restriction will force the compiler to attempt to coerce bigNum to a concrete type. In this case, the compiler infers recursively from the (^) function that its arguments should be a Num and an Integer, and since the output of (^) would in this case be a Num, it tries to find a Num instance for the fully function type. One possible way to fix both is to give bigNum a type assignment and not apply it to 10.

> bigNum = (^) 5 :: (Num a) => Integer -> a
> wahoo = bigNum $ 10

2. x = print
y = print "woohoo!"
z = x "hello world"

This should compile just fine.

3.
a = (+)
b = 5
c = b 10
d = c 200

This will not compile because b is not a function. Also, the compiler (because of the same restriction) is going to complain about there not being a Num instance for the types of a (t1 -> t1 -> t1), c (t1 -> t1), b (t2), and d (t1). We can clean up like this:

> a = (+)
> b = 5 :: Integer
> c = a (10 :: Integer)
> d = c 200 :: Integer

4.
m = 12 + n
n = 10000 * o

This does not compile because o is "not in scope".

We can fix it like this:

> m = 12 + n
> n = 10000 * o
> o = 44



Type variable or specific type constructor?
1. You will be shown a type declaration, and you should categorize each type. The choices are a fully polymorphic type variable, constrained polymorphic type variable, or concrete type constructor.

f :: Num a => a -> b -> Int -> Int

Here, a is constrained polymorphic, b is fully polymorphic, and the Int arguments are concrete type constructors.

2. f :: zed -> Zed -> Blah

zed is fully polymorphic, Zed is concrete, and Blah is concrete too.

3. f :: Enum b => a -> b -> c

a and c are fully polymorphic, and b is constrained.

4. f :: f -> g -> C

f and g are fully polymorphic, and C is concrete.



Write a type signature
For the following expressions, please add a type signature. You should be able to rely on GHCi type inference to check your work, although you might not have precisely the same answer as GHCi gives (due to polymorphism, etc).
1. While we haven’t fully explained this syntax yet, you’ve seen it in Chapter 2 and as a solution to an exercise in Chapter 4. This syntax is a way of destructuring a single element of a list by pattern matching.

> functionH :: [a] -> a
> functionH (x:_) = x

2.

> functionC :: Ord a => a -> a -> Bool
> functionC x y =
>   if (x > y) then True else False

3.

> functionS :: (a, b) -> b
> functionS (x, y) = y



Given a type, write the function
You will be shown a type and a function that needs to be written. Use the information the type provides to determine what the function should do. We’ll also tell you how many ways there are to write the function. Syntactically different but semantically equivalent implementations are not counted as being different. For example, writing a function one way then rewriting the semantically identical function but using anonymous lambda syntax does not count as two implementations.

1. There is only one function definition that typechecks and doesn’t go into an infinite loop when you run it.

i :: a -> a
i = undefined

> funI :: a -> a
> funI a = a

2. There is only one version that works.

c :: a -> b -> a
c = undefined

> funC :: a -> b -> a
> funC a b = a

3. Given alpha equivalence are c'' and c (see above) the same thing?

c'' :: b -> a -> b
c'' = ?

> funC'' :: b -> a -> b
> funC'' b a = b

Yes c'' is the same as c given alpha equivalence.

4. Only one version that works.

c' :: a -> b -> b
c' = undefined

> funC' :: a -> b -> b
> funC' a b = b

5. There are multiple possibilities, at least two of which you’ve seen in previous chapters.

r :: [a] -> [a]
r = undefined

> r :: [a] -> [a]
> r = reverse

6. Only one version that will typecheck.

co :: (b -> c) -> (a -> b) -> a -> c
co = undefined

> co :: (b -> c) -> (a -> b) -> a -> c
> co = (.)

7. One version will typecheck.

a :: (a -> c) -> a -> a
a = undefined

> funA :: (a -> c) -> a -> a
> funA _ a = a

8. One version will typecheck.

a' :: (a -> b) -> a -> b
a' = undefined

> funA' :: (a -> b) -> a -> b
> funA' = ($)



Fix it :

Won’t someone take pity on this poor broken code and fix it up? Be sure to check carefully for things like capitalization, parentheses, and indentation.
1. module sing where
fstString :: [Char] ++ [Char]
     fstString x = x ++ " in the rain"
     sndString :: [Char] -> Char
     sndString x = x ++ " over the rainbow"
     sing = if (x > y) then fstString x or sndString y
     where x = "Singin"
           x = "Somewhere"
2. Now that it’s fixed, make a minor change and make it sing the other song. If you’re lucky, you’ll end up with both songs stuck in your head!
3. -- arith3broken.hs
module Arith3Broken where
main :: IO () Main = do
print 1 + 2
putStrLn 10
print (negate -1) print ((+) 0 blah) where blah = negate 1

For answers, see fixIt1.hs, arith3broken.hs


Type-Kwon-Do
The name is courtesy of Phillip Wright.4 Thank you for the idea! The focus here is on manipulating terms in order to get the types to fit. This sort of exercise is something you’ll encounter in writing real Haskell code, so the practice will make it easier to deal with when you get there. Practicing this will make you better at writing ordinary
code as well.
We provide the types and bottomed out (declared as undefined)
terms. Bottom and undefined will be explained in more detail later. The contents of the terms are irrelevant here. You’ll use only the declarations provided and what the Prelude provides by default unless otherwise specified. Your goal is to make the ???’d declaration pass the typechecker by modifying it alone.

See typeKwonDo1.hs.
