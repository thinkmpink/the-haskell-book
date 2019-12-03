Chapter 10. Folding Lists

> module FoldingLists where
>
> import Data.Bool

Exercises: Understanding Folds
1. foldr (*) 1 [1..5]
will return the same result as which of the following:
a) flip (*) 1 [1..5]
b) foldl (flip (*)) 1 [1..5]
c) foldl (*) 1 [1..5]

a) will fail to typecheck, but both b) and c) will return the same result as the original.

2. Write out the evaluation steps for
foldl (flip (*)) 1 [1..3]

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc []     = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

foldl (flip (*)) 1 [1..3]
 = foldl (flip (*)) (flip (*) 1 1) [2..3]
 = foldl (flip (*)) ((flip (*) (flip (*) 1 1) 2)) [3]
 = foldl (flip (*)) (((flip (*) ((flip (*) ((flip (*) 1 1) 2) 3))))) []
 = flip (*) ((flip (*) ((flip (*) 1 1)) 2)) 3
 = flip (*) ((flip (*) ((1)) 2)) 3
 = flip (*) (2 * 1) 3
 = flip (*) 2 3
 = 3 * 2
 = 6

3. One difference between foldr and foldl is:
a) foldr, but not foldl, traverses the spine of a list from right to left
b) foldr, but not foldl, always forces the rest of the fold
c) foldr, but not foldl, associates to the right
d) foldr, but not foldl, is recursive

a), b), and d) are false. c) however, is correct.

4. Folds are catamorphisms, which means they are generally used
to
a) reduce structure
b) expand structure
c) render you catatonic
d) generate infinite data structures

a) reduce structure. While they could be used to do b) or d), they are not as common.


5. The following are simple folds very similar to what you’ve already seen, but each has at least one error. Please fix them and test in your REPL:

a) foldr (++) ["woot", "WOOT", "woot"]
mp> foldr (++) "" ["woot", "WOOT", "woot"]
"wootWOOTwoot"

b) foldr max [] "fear is the little death"
mp> foldr max 'a' "fear is the little death"
't'

c) foldr and True [False, True]
mp> foldr (&&) True [False, True]
False

d) This one is more subtle than the previous. Can it ever return a different answer?
foldr (||) True [False, True]

No, it cannot return a different answer because anything or'ed with True is True. A more interesting expression is:
mp> foldr (||) True [False, True]
True

e) foldl ((++) . show) "" [1..5]
 = foldl ((++) . show) (((++) . show) "" 1) [2..5]

To concatenate in the order of the input`, do a right fold:
mp> foldr ((++) . show) "" [1..5]
"12345"

To still use a left fold but reverse the output, flip the input to the folding function:
mp> foldl (flip ((++) . show)) "" [1..5]
"54321"

To use a left fold without flipping the order of the input, desugar from pointfree style:
mp> foldl (\b a -> b ++ show a) "" [1..5]
"12345"

f) foldr const 'a' [1..5]
The following are valid replacements:
mp> foldr const 0 [1..5]
1
mp> foldr (flip const) 'a' [1..5]
'a'
mp> foldl const 'a' [1..5]
'a'

g) foldr const 0 "tacos"
Similarly, here are some valid replacements:
mp> foldr const 'a' "tacos"
't'
mp> foldr (flip const) 0 "tacos"
0
mp> foldl const 0 "tacos"
0

h) foldl (flip const) 0 "burritos"
mp> foldl (flip const) 'z' "burritos"
's'

i) foldl (flip const) 'z' [1..5]
mp> foldl const 'z' [1..5]
'z'



Exercises: Database Processing
Write the following functions for processing this data.

> import Data.Time
> data DatabaseItem = DbString String
>                   | DbNumber Integer
>                   | DbDate   UTCTime
>                   deriving (Eq, Ord, Show)
>
> theDatabase :: [DatabaseItem]
> theDatabase =
>   [ DbDate (UTCTime
>             (fromGregorian 1911 5 1)
>             (secondsToDiffTime 34123))
>   , DbNumber 9001
>   , DbString "Hello, world!"
>   , DbDate (UTCTime
>             (fromGregorian 1921 5 1)
>             (secondsToDiffTime 34123))
>   ]



1. Write a function that filters for DbDate values and returns a list
of the UTCTime values inside them.

> filterDbDate :: [DatabaseItem] -> [UTCTime]
> filterDbDate = foldr addDate []
>   where addDate (DbDate t) ts = t:ts
>         addDate _          ts = ts

2. Write a function that filters for DbNumber values and returns a list
of the Integer values inside them.

> filterDbNumber :: [DatabaseItem] -> [Integer]
> filterDbNumber = foldr addNum []
>   where addNum (DbNumber n) ns = n:ns
>         addNum _            ns = ns


3. Write a function that gets the most recent date.

> mostRecent :: [DatabaseItem] -> UTCTime
> mostRecent = maximum . filterDbDate

4. Write a function that sums all of the DbNumber values.

> sumDb :: [DatabaseItem] -> Integer
> sumDb = sum . filterDbNumber

5. Write a function that gets the average of the DbNumber values.

> -- You'll probably need to use fromIntegral
> -- to get from Integer to Double.
> avgDb :: [DatabaseItem] -> Double
> avgDb xs = fromIntegral (sumDb xs) /
>   fromIntegral (length (filterDbNumber xs))

Scans Exercises

> fibs = 1 : scanl (+) 1 fibs

1. Modify your fibs function to only return the first 20 Fibonacci numbers.

> fibs' = take 20 (1: scanl (+) 1 fibs')

2. Modify fibs to return the Fibonacci numbers that are less than 100.

> fibs'' = takeWhile (<100) (1: scanl (+) 1 fibs'')

3. Try to write the factorial function from Recursion as a scan. You’ll want scanl again, and your start value will be 1. Warning: this will also generate an infinite list, so you may want to pass it through a take function or similar.

> fac = scanl (*) 1 [1..]



10.10 Chapter Exercises Warm-up and review
For the following set of exercises, you are not expected to use folds. These are intended to review material from previous chapters. Feel free to use any syntax or structure from previous chapters that seems appropriate.
1. Given the following sets of consonants and vowels:
     stops  = "pbtdkg"
     vowels = "aeiou"
a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations. These will not all correspond to real words in English, although the stop-vowel-stop pattern is common enough that many of them will.

> stops  = "pbtdkg"
> vowels = "aeiou"

> stopVowelStops :: [String]
> stopVowelStops = [[x, y, z] | x <- stops, y <- vowels, z <- stops]

b) Modify that function so that it only returns the combinations that begin with a p.

> stopVowelStopsWithP :: [String]
> stopVowelStopsWithP = [['p', y, z] | y <- vowels, z <- stops]

c) Now set up lists of nouns and verbs (instead of stops and vowels) and modify the function to make tuples representing possible noun-verb-noun sentences.

> nouns = [ "brick", "light", "exit", "glasses"
>         , "poster", "blanket", "vine", "socket"
>         ]

> verbs = [ "types on", "sits on", "chews on", "light"
>         , "drapes over", "exits", "tell"
>         ]

> nounVerbNouns = [[n1, v, n2] | n1 <- nouns, v <- verbs, n2 <- nouns]


2. What does the following mystery function do? What is its type? Try to get a good sense of what it does before you test it in the REPL to verify it.

> seekritFunc x =
>   div (sum (map length (words x)))
>            (length (words x))

seekritFunc :: [Char] -> Int
This function gets the average length of words in a String. While div, sum, and map are all polymorphic in their output type, words must consume a String, meaning that the variable x has the expected type of String, and length returns an Int, meaning that div will return an Int.

*FoldingLists> :t seekritFunc
seekritFunc :: String -> Int
*FoldingLists> seekritFunc "Hello my name is Michael"
4
*FoldingLists> :t it
it :: Int

3. We’d really like the answer to be more precise. Can you rewrite that using fractional division?

> seekritFunc' :: (Fractional a) => String -> a
> seekritFunc' x = s / n
>   where s = fromIntegral . sum . map length . words $ x
>         n = fromIntegral . length . words $ x


Rewriting functions using folds
In the previous chapter, you wrote these functions using direct recursion over lists. The goal now is to rewrite them using folds. Where possible, to gain a deeper understanding of folding, try rewriting the fold version so that it is point-free.

1. myOr returns True if any Bool in the list is True.

> myOr :: [Bool] -> Bool
> myOr = foldr (||) False

> myOr' :: [Bool] -> Bool
> myOr' = foldr (\a acc -> a || acc) False

2. myAny returns True if a -> Bool applied to any of the values in the list returns True.

> myAny :: (a -> Bool) -> [a] -> Bool
> myAny p = foldr ((||) . p) False

> myAny' p = foldr (\a acc -> p a || acc) False

Example for validating myAny:
Prelude> myAny even [1, 3, 5]
False
Prelude> myAny odd [1, 3, 5]
True

3. Write two versions of myElem. One version should use folding and the other should use any.

> myElem :: Eq a => a -> [a] -> Bool
> myElem e = foldr ((||) . (== e)) False

> myElem' e = any (== e)

Prelude> myElem 1 [1..10]
True
Prelude> myElem 1 [2..10]
False

4. Implement myReverse, don’t worry about trying to make it lazy.

> myReverse :: [a] -> [a]
> myReverse = foldr (flip (++) . return) []

Prelude> myReverse "blah"
"halb"
Prelude> myReverse [1..5]
[5,4,3,2,1]

5. Write myMap in terms of foldr. It should have the same behavior as the built-in map.

> myMap :: (a -> b) -> [a] -> [b]
> myMap f = foldr ((:) . f) []

6. Write myFilter in terms of foldr. It should have the same behavior as the built-in filter.

> myFilter :: (a -> Bool) -> [a] -> [a]
> myFilter p = foldr (\a acc -> if p a then (a:acc) else acc) []


7. squish flattens a list of lists into a list

> squish :: [[a]] -> [a]
> squish = foldr (++) []

8. squishMap maps a function over a list and concatenates the results.

> squishMap :: (a -> [b]) -> [a] -> [b]
> squishMap f = foldr ((++) . f) []

9. squishAgain flattens a list of lists into a list. This time re-use the squishMap function.

> squishAgain :: [[a]] -> [a]
> squishAgain = squishMap id

10. myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returned GT for.

> myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
> myMaximumBy cmp = foldr1 (\a acc -> if cmp a acc == GT then a else acc)

Prelude> myMaximumBy (\_ _ -> GT) [1..10]
1
Prelude> myMaximumBy (\_ _ -> LT) [1..10]
10
Prelude> myMaximumBy compare [1..10]
10

11. myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returned LT for.

> myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
> myMinimumBy cmp = foldr1 (\a acc -> if cmp a acc == LT then a else acc)

Prelude> myMinimumBy (\_ _ -> GT) [1..10]
10
