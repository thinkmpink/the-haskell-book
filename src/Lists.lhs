Chapter 9. Lists.

> module Lists where
>
> import Data.Bool
> import Data.Char

Exercise: EnumFromTo

Write your own enumFromTo definitions for the types provided. Do not use range syntax to do so. It should return the same results as if you did [start..stop]. Replace the undefined, an value which results in an error when evaluated, with your own definition.

> eft :: (Ord a, Enum a) => a -> a -> [a]
> eft a b
>   | a > b     = []
>   | a == b    = [b]
>   | otherwise = a: eft (succ a) b

> eftBool :: Bool -> Bool -> [Bool]
> eftBool = eft

> eftOrd :: Ordering
>        -> Ordering
>        -> [Ordering]
> eftOrd = eft

> eftInt :: Int -> Int -> [Int]
> eftInt = eft

> eftChar :: Char -> Char -> [Char]
> eftChar = eft


Exercises: Thy Fearful Symmetry
1. Using takeWhile and dropWhile, write a function that takes a string and returns a list of strings, using spaces to separate the elements of the string into words, as in the following sample:
     Prelude> myWords "sheryl wants fun"
     ["sheryl", "wants", "fun"]

> myWords :: String -> [String]
> myWords "" = []
> myWords s  = takeWhile (/= ' ') s
>            : (myWords
>            . dropWhile (== ' ')
>            . dropWhile (/= ' ') $ s)

2. Next, write a function that takes a string and returns a list of strings, using newline separators to break up the string as in the following (your job is to fill in the undefined function).

see PoemLines.hs

3. Now let’s look at what those two functions have in common. Try writing a new function that parameterizes the character you’re breaking the string argument on and rewrite myWords and myLines using it.

> mySep :: Char -> String -> [String]
> mySep c "" = []
> mySep c s = takeWhile (/= c) s
>           : (mySep c
>           . dropWhile (== c)
>           . dropWhile (/= c) $ s)

> myWords' :: String -> [String]
> myWords' = mySep ' '

> myLines' :: String -> [String]
> myLines' = mySep '\n'

Exercises: Comprehend Thy Lists
Take a look at the following functions, figure what you think the output lists will be, and then run them in your REPL to verify (note that you will need the mySqr list from above in scope to do this):

> mySqr = [x^2 | x <- [1..10]]
> -- [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

[x | x <- mySqr, rem x 2 == 0]

evaluates to:
= [4, 16, 36, 64, 100]

[(x, y) | x <- mySqr
        , y <- mySqr
        , x < 50
        , y > 50
        ]

evaluates to:
[ (1, 64), (1, 81), (1, 100)
, (4, 64), (4, 81), (4, 100)
, (9, 64), (9, 81), (9, 100)
, (16, 64), (16, 81), (16, 100)
, (25, 64), (25, 81), (25, 100)
, (36, 64), (36, 81), (36, 100)
, (49, 64), (49, 81), (49, 100)
]


take 5 [(x, y) | x <- mySqr
               , y <- mySqr
               , x < 50
               , y > 50
               ]
evaluates to:
[ (1, 64), (1, 81), (1, 100)
, (4, 64), (4, 81)
]


Exercises: Square Cube
Given the following:
Prelude> mySqr = [x^2 | x <- [1..5]]
Prelude> myCube = [y^3 | y <- [1..5]]
1. First write an expression that will make tuples of the outputs of mySqr and myCube.

[(x, y) | x <- mySqr, y <- myCube]

2. Now alter that expression so that it only uses the x and y values that are less than 50.

[(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

3. Apply another function to that list comprehension to determine how many tuples inhabit your output list.

length $ [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]


Exercises: Bottom Madness
Will it blow up?
Will the following expressions return a value or be ⊥?

1. [x^y | x <- [1..5], y <- [2, undefined]]
blows up (we need to evaluate the value of the exponent) in order to evaluate the whole list

2. take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
returns a value because take 1 ensures we never look at the value that would be bottom.

3. sum [1, undefined, 3]
blows up because sum evaluates the cells, not just the spine

4. length [1, 2, undefined]
returns a value because it only needs to evaluate the spine

5. length $ [1, 2, 3] ++ undefined
blows up because the spine of the concat'ed list is bottom.

6. take 1 $ filter even [1, 2, 3, undefined]
returns a value because take 1 only needs to evaluate two of the values in the list to return a value.

7. take 1 $ filter even [1, 3, undefined]
blows up because take, while looking through the list to (unsuccessfully) get an even value, will evaluate bottom.

8. take 1 $ filter odd [1, 3, undefined]
will return a value for the same reason as 6.

9. take 2 $ filter odd [1, 3, undefined]
will return a value for the same reason as 6 and 8.

10. take 3 $ filter odd [1, 3, undefined]
will blow up.


Intermission: Is it in normal form?
For each expression below, determine whether it’s in:
1. normal form, which implies weak head normal form; 2. weak head normal form only; or,
3. neither.
Remember that an expression cannot be in normal form or weak head normal form if the outermost part of the expression isn’t a data constructor. It can’t be in normal form if any part of the expression is unevaluated.
1. [1, 2, 3, 4, 5]
is in WHNF and NF because it is fully evaluated.

2. 1 : 2 : 3 : 4 : _
is in WHNF but not NF.

3. enumFromTo 1 10
is in neither WHNF nor NF.

4. length [1, 2, 3, 4, 5]
is in neither WHNF nor NF.

5. sum (enumFromTo 1 10)
is in neither WHNF nor NF.

6. ['a'..'m'] ++ ['n'..'z']
is in neither WHNF nor NF.

7. (_, 'b')
is in WHNF but not NF



Exercises: More Bottoms
As always, we encourage you to try figuring out the answers before you enter them into your REPL.

1. Will the following expression return a value or be ⊥? take 1 $ map (+1) [undefined, 2, 3]
This will be bottom because take 1 will force the evaluation of the first cell of the list, which is bottom.

2. Will the following expression return a value?
take 1 $ map (+1) [1, undefined, 3]
Yes, this will return a value because the first element of the input list is defined.

3. Will the following expression return a value?
take 2 $ map (+1) [1, undefined, 3]
This will return bottom, because take 2 will force the evaluation of the function applied to the second element of the list.

4. What does the following mystery function do? What is its type? Describe it (to yourself or a loved one) in standard English and then test it out in the REPL to make sure you were correct.

> itIsMystery xs =
>   map (\x -> elem x "aeiou") xs

This will map a [Char] to a [Bool]. Vowels are mapped to True, and everything else to False. Its type is itIsMystery :: [Char] -> [Bool].

5. What will be the result of the following functions: a) map (^2) [1..10]
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

b) map minimum [[1..10], [10..20], [20..30]] -- n.b. `minimum` is not the same function -- as the `min` that we used before
[1, 10, 20]

c) map sum [[1..5], [1..5], [1..5]]
[15, 15, 15]


6. Back in chapter 7, you wrote a function called foldBool. That function exists in a module known as Data.Bool and is called bool. Write a function that does the same (or similar, if you wish) as the map (if-then-else) function you saw above but uses bool instead of the if-then-else syntax. Your first step should be bringing the bool function into scope by typing import Data.Bool at your Prelude prompt.

Original:
Prelude> map (\x -> if x == 3 then (-x) else (x)) [1..10]
[1,2,-3,4,5,6,7,8,9,10]

New:

> negate3 :: (Eq a, Num a) => [a] -> [a]
> negate3 = map (\x -> bool x (-x) (x==3))



Exercises: Filtering
1. Given the above, how might we write a filter function that would give us all the multiples of 3 out of a list from 1-30?

> filterIsMult3 :: Integral a => [a] -> [a]
> filterIsMult3 = filter (\x -> x `mod` 3 == 0)

2. Recalling what we learned about function composition, how could we compose the above function with the length function to tell us *how many* multiples of 3 there are between 1 and 30?

We could write:

> numMult3 = length . filterIsMult3

3. Next we’re going to work on removing all articles (’the’, ’a’, and ’an’) from sentences. You want to get to something that works like this:
     Prelude> myFilter "the brown dog was a goof"
     ["brown","dog","was","goof"]

> noArticles :: String -> [String]
> noArticles = filter (not . flip elem ["the", "a", "an"]) . words



Zipping exercises
1. Write your own version of zip and ensure it behaves the same as the original.

> zip' :: [a] -> [b] -> [(a, b)]
> zip' []     _      = []
> zip' _      []     = []
> zip' (a:as) (b:bs) = (a, b):zip' as bs

2. Do what you did for zip, but now for zipWith:

> zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
> zipWith' _ []     _      = []
> zipWith' _ _      []     = []
> zipWith' f (a:as) (b:bs) = f a b:zipWith' f as bs

3. Rewrite your zip in terms of the zipWith you wrote.

> zip'' = zipWith' (,)


9.12 Chapter Exercises

1. Query the types of isUpper and toUpper.
isUpper :: Char -> Bool
toUpper :: Char -> Char

2. Given the following behaviors, which would we use to write a function that filters all the uppercase letters out of a String? Write that function such that, given the input “HbEfLrLxO,” your function will return “HELLO.”

> filterUpper :: String -> String
> filterUpper = filter isUpper

3. Write a function that will capitalize the first letter of a string and return the entire string. For example, if given the argument
“julie,” it will return “Julie.”

> capitalize :: String -> String
> capitalize (a:as) = toUpper a :as
> capitalize _      = []

4. Now make a new version of that function that is recursive such that if you give it the input “woot” it will holler back at you “WOOT.” The type signature won’t change, but you will want to
add a base case.

> capitalizeR :: String -> String
> capitalizeR (a:as) = toUpper a : capitalizeR as
> capitalizeR _      = []

5. To do the final exercise in this section, we’ll need another standard function for lists called head. Query the type of head and experiment with it to see what it does. Now write a function that will capitalize the first letter of a String and return only that letter as the result.

> firstThenCap :: String -> Char
> firstThenCap s = toUpper (head s)

6. Cool. Good work. Now rewrite it as a composed function. Then, for fun, rewrite it pointfree.

> firstThenCap' s = toUpper . head $ s

> firstThenCap'' = toUpper . head

Ciphers
We’ll still be using Data.Char for this next exercise. You should save these exercises in a module called Cipher because we’ll be coming back to them in later chapters.

see Cipher.lhs


Writing your own standard functions
Below are the outlines of some standard functions. The goal here is to write your own versions of these to gain a deeper understanding of recursion over lists and how to make functions flexible enough to accept a variety of inputs.

> -- direct recursion, not using (&&)
> myAnd :: [Bool] -> Bool
> myAnd [] = True
> myAnd (x:xs) =
>   if x == False
>   then False
>   else myAnd xs

And now the fun begins:
1. myOr returns True if any Bool in the list is True.

> myOr :: [Bool] -> Bool
> myOr []     = False
> myOr (x:xs) = x || myOr xs

2. myAny returns True if a -> Bool applied to any of the values in the list returns True.

> myAny :: (a -> Bool) -> [a] -> Bool
> myAny p = myOr . map p

> myAnyAnnoying :: (a -> Bool) -> [a] -> Bool
> myAnyAnnoying p (x:xs) = p x || myAnyAnnoying p xs
> myAnyAnnoying _ []     = False

3. After you write the recursive myElem, write another version that uses any. The built-in version of elem in GHC 7.10 and newer has a type that uses Foldable instead of the list type specifically. You can ignore that and write the concrete version that works only for list.
myElem :: Eq a => a -> [a] -> Bool
     Prelude> myElem 1 [1..10]
     True
     Prelude> myElem 1 [2..10]
     False

> myElem :: Eq a => a -> [a] -> Bool
> myElem x (a:as) = x == a || myElem x as
> myElem x _      = False

> myElem' :: Eq a => a -> [a] -> Bool
> myElem' x = any (==x)

4. Implement myReverse.

> myReverse :: [a] -> [a]
> myReverse []     = []
> myReverse (x:xs) = myReverse xs ++ [x]

     Prelude> myReverse "blah"
     "halb"
     Prelude> myReverse [1..5]
     [5,4,3,2,1]

5. squish flattens a list of lists into a list

> squish :: [[a]] -> [a]
> squish (a:as) = a ++ squish as
> squish _      = []


6. squishMap maps a function over a list and concatenates the results.

> squishMap :: (a -> [b]) -> [a] -> [b]
> squishMap f = squish . map f

     Prelude> squishMap (\x -> [1, x, 3]) [2]
     [1,2,3]
     Prelude> squishMap (\x -> "WO "++[x]++" HOO ") "123"
     "WO 1 HOO WO 2 HOO WO 3 HOO "


7. squishAgain flattens a list of lists into a list. This time re-use the squishMap function.

> squishAgain :: [[a]] -> [a]
> squishAgain = squishMap id

8. myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returned GT for. If you import maximumBy from Data.List, you’ll see the type is:

Foldable t => (a -> a -> Ordering) -> t a -> a

rather than

(a -> a -> Ordering) -> [a] -> a

> myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
> myMaximumBy cmp as = go (head as) as
>   where go curMax (x:xs)
>           | cmp curMax x == LT = go x xs
>           | otherwise          = go curMax xs
>         go curMax _            = curMax

     Prelude> xs = [1, 53, 9001, 10]
     Prelude> myMaximumBy compare xs
     9001

9. myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returned LT for.

> myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
> myMinimumBy cmp = myMaximumBy (flip cmp)

     Prelude> xs = [1, 53, 9001, 10]
     Prelude> myMinimumBy compare xs
     1

10. Using the myMinimumBy and myMaximumBy functions, write your own versions of maximum and minimum. If you have GHC 7.10 or newer, you’ll see a type constructor that wants a Foldable instance in- stead of a list as has been the case for many functions so far.

> myMaximum :: (Ord a) => [a] -> a
> myMaximum = myMaximumBy compare

> myMinimum :: (Ord a) => [a] -> a
> myMinimum = myMinimumBy compare
