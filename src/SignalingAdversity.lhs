Chapter 12

> module SignalingAdversity where

Chapter Exercises

1. Given
id :: a -> a
What is the kind of a?

The kind of a is *. It's some concrete, lifted type.

2. r :: a -> f a

What are the kinds of a and f?

The kind of a is *. The kind of f is * -> *.


String processing.

1. Write a recursive function named replaceThe which takes a text/string, breaks it into words and replaces each instance of “the” with “a”. It’s intended only to replace exactly the word “the”. notThe is a suggested helper function for accomplishing this.

> notThe :: String -> Maybe String
> notThe "the" = Nothing
> notThe s     = Just s

Example GHCi session using the above functions:

Prelude> notThe "the"
Nothing
Prelude> notThe "blahtheblah"
Just "blahtheblah"
Prelude> notThe "woot"
Just "woot"

> replaceThe :: String -> String
> replaceThe = unwords . go . words
>   where go []     = []
>         go (w:ws) = case notThe w of
>           Nothing -> "a" : go ws
>           Just s  -> s : go ws


An alternate implementation:

> replaceThe' :: String -> String
> replaceThe' = unwords
>             . map (maybe "a" id . notThe)
>             . words


Prelude> replaceThe "the cow loves us"
"a cow loves us"


2. Write a recursive function that takes a text/string, breaks it into words, and counts the number of instances of ”the” followed by a vowel-initial word.

> beginsWithVowel :: String -> Bool
> beginsWithVowel ""     = False
> beginsWithVowel (a:as) = elem a "aeiou"
>
> countTheBeforeVowel :: String -> Integer
> countTheBeforeVowel = countTheVowel 0 False . words
>   where countTheVowel :: Integer -> Bool -> [String] -> Integer
>         countTheVowel n _ []  = n
>         countTheVowel n False (w:ws)
>           = countTheVowel n (notThe w == Nothing) ws
>         countTheVowel n True (w:ws) = case notThe w of
>           Just s -> case beginsWithVowel w of
>                       True -> countTheVowel (n+1) False ws
>                       False -> countTheVowel n False ws
>           Nothing -> countTheVowel n True ws

Prelude> countTheBeforeVowel "the cow"
0
Prelude> countTheBeforeVowel "the evil cow"
1

I'm not too happy with this solution because the nested case statements are pretty ugly. Let's try to emulate the book's error handling. Remembering that notThe :: String -> Maybe String,

> beginsWithVowel' :: String -> Maybe String
> beginsWithVowel' []     = Nothing
> beginsWithVowel' word@(a:as)
>   | elem a "aeiou"      = Just word
>   | otherwise           = Nothing
>
> countTheBeforeVowel' :: String -> Integer
> countTheBeforeVowel' = go 0 . words
>   where
>     go n (a:b:cs)
>       | notThe a == Nothing
>         && beginsWithVowel' b /= Nothing = go (n+1) cs
>       | otherwise                        = go n (b:cs)
>     go n _                               = n

3. Return the number of letters that are vowels in a word.
Hint: it’s helpful to break this into steps. Add any helper functions necessary to achieve your objectives.

a) Test for vowelhood
b) Return the vowels of a string
c) Count the number of elements returned

> countVowels :: String -> Integer
> countVowels = fromIntegral
>             . length
>             . filter (flip elem "aeiou")

Prelude> countVowels "the cow"
2
Prelude> countVowels "Mikolajczak"
4


Validate the word
Use the Maybe type to write a function that counts the number of vowels in a string and the number of consonants. If the number of vowels exceeds the number of consonants, the function returns Nothing. In many human languages, vowels rarely exceed the number of consonants so when they do, it may indicate the input isn’t a word (that is, a valid input to your dataset):

> newtype Word' =
>   Word' String
>   deriving (Eq, Show)

> vowels = "aeiou"

> mkWord :: String -> Maybe Word'
> mkWord s
>   | numVowels s > numConsonants s = Nothing
>   | otherwise                     = Just (Word' s)
>   where numVowels = fromIntegral . countVowels
>         numConsonants w = length w - numVowels w


It’s only Natural

You’ll be presented with a datatype to represent the natural numbers. The only values representable with the naturals are whole numbers from zero to infinity. Your task will be to implement functions to convert Naturals to Integers and Integers to Naturals. The conversion from Naturals to Integers won’t return Maybe because Integer is a strict superset of Natural. Any Natural can be represented by an Integer, but the same is not true of any Integer. Negative numbers are not valid natural numbers.

> -- As natural as any
> -- competitive bodybuilder
>
> data Nat =
>     Zero
>   | Succ Nat
>   deriving (Eq, Show)

> natToInteger :: Nat -> Integer
> natToInteger Zero     = 0
> natToInteger (Succ n) = natToInteger n + 1

> integerToNat :: Integer -> Maybe Nat
> integerToNat n
>   | n < 0     = Nothing
>   | otherwise = Just (go n)
>   where go i
>          | i == 0    = Zero
>          | otherwise = Succ (go $ i-1)

Prelude> natToInteger Zero
0
Prelude> natToInteger (Succ Zero)
1
Prelude> natToInteger (Succ (Succ Zero))
2
Prelude> integerToNat 0
Just Zero
Prelude> integerToNat 1
Just (Succ Zero)
Prelude> integerToNat 2
Just (Succ (Succ Zero))
Prelude> integerToNat (-1)
Nothing


Small library for Maybe
Write the following functions. This may take some time.

1. Simple boolean checks for Maybe values.

> isJust :: Maybe a -> Bool
> isJust (Just _) = True
> isJust _        = False

> isNothing :: Maybe a -> Bool
> isNothing Nothing = True
> isNothing _       = False

Prelude> isJust (Just 1)
True
Prelude> isJust Nothing
False
Prelude> isNothing (Just 1)
False
Prelude> isNothing Nothing
True



2. The following is the Maybe catamorphism. You can turn a Maybe value into anything else with this.

> mayybee :: b -> (a -> b) -> Maybe a -> b
> mayybee b f (Just a) = f a
> mayybee b _ _        = b

Prelude> mayybee 0 (+1) Nothing
0
Prelude> mayybee 0 (+1) (Just 1)
2

3. In case you just want to provide a fallback value. Try writing it in terms of the maybe catamorphism.

> fromMaybe :: a -> Maybe a -> a
> fromMaybe = flip mayybee id

Prelude> fromMaybe 0 Nothing
0
Prelude> fromMaybe 0 (Just 1)
1

4. Converting between List and Maybe.

> listToMaybe :: [a] -> Maybe a
> listToMaybe []    = Nothing
> listToMaybe (a:_) = Just a

> maybeToList :: Maybe a -> [a]
> maybeToList (Just a) = [a]
> maybeToList _        = []

Prelude> listToMaybe [1, 2, 3]
Just 1
Prelude> listToMaybe []
Nothing
Prelude> maybeToList (Just 1)
[1]
Prelude> maybeToList Nothing
[]

5. For when we want to drop the Nothing values from our list.

> catMaybes :: [Maybe a] -> [a]
> catMaybes =
>   foldr (\a acc -> case a of
>           Just b -> b:acc
>           _      ->   acc
>         ) []

Prelude> catMaybes [Just 1, Nothing, Just 2]
[1, 2]
Prelude> let xs = take 3 $ repeat Nothing
Prelude> catMaybes xs
[]

6. You’ll see this called “sequence” later.

> flipMaybe :: [Maybe a] -> Maybe [a]
> flipMaybe =
>   foldr (\m acc -> case m of
>            Nothing -> Nothing
>            Just a  -> fmap (a:) acc
>         )
>         (Just [])

Prelude> flipMaybe [Just 1, Just 2, Just 3]
Just [1, 2, 3]
Prelude> flipMaybe [Just 1, Nothing, Just 3]
Nothing


Small library for Either
Write each of the following functions. If more than one possible unique function exists for the type, use common sense to determine what it should do.

1. Try to eventually arrive at a solution that uses foldr, even if earlier versions don’t use foldr.

> lefts' :: [Either a b] -> [a]
> lefts' =
>   foldr (\a acc -> case a of
>            Left a' -> a':acc
>            _       ->    acc
>         ) []

2. Same as the last one. Use foldr eventually.

> rights' :: [Either a b] -> [b]
> rights' =
>   foldr (\a acc -> case a of
>            Right a' -> a':acc
>            _        ->    acc
>         ) []

3.

> partitionEithers' :: [Either a b] -> ([a], [b])
> partitionEithers' = foldr partition ([], [])
>   where partition (Left a) (as, bs)  = (a:as, bs)
>         partition (Right b) (as, bs) = (as, b:bs)

4.

> eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
> eitherMaybe' f (Right b) = Just (f b)
> eitherMaybe' _ (Left a)  = Nothing

5. This is a general catamorphism for Either values.

> either' :: (a -> c)
>         -> (b -> c)
>         -> Either a b
>         -> c
> either' f _ (Left a)  = f a
> either' _ g (Right b) = g b

6. Same as before, but use the either' function you just wrote.

> eitherMaybe'' :: (b -> c)
>               -> Either a b
>               -> Maybe c
> eitherMaybe'' f = either' (const Nothing) (Just . f)



Write your own iterate and unfoldr
1. Write the function myIterate using direct recursion. Compare the behavior with the built-in iterate to gauge correctness. Do not look at the source or any examples of iterate so that you are forced to do this yourself.

> myIterate :: (a -> a) -> a -> [a]
> myIterate f a = a: myIterate f (f a)

2. Write the function myUnfoldr using direct recursion. Compare with the built-in unfoldr to check your implementation. Again, don’t look at implementations of unfoldr so that you figure it out yourself.

> myUnfoldr :: (b -> Maybe (a, b))
>           -> b
>           -> [a]
> myUnfoldr un b = go (un b)
>   where go (Just (a', b')) = a':myUnfoldr un b'
>         go _               = []
>
> testUn :: Int -> Maybe (String, Int)
> testUn i
>   | i < 100   = Just (take i $ repeat 'h', i+10)
>   | otherwise = Nothing

3. Rewrite myIterate into betterIterate using myUnfoldr.

> betterIterate :: (a -> a) -> a -> [a]
> betterIterate f a =
>   myUnfoldr (\b -> Just (b, f b)) a


Finally something other than a list!

Given the BinaryTree from last chapter, complete the following exercises. Here’s that datatype again:

> data BinaryTree a =
>     Leaf
>   | Node (BinaryTree a) a (BinaryTree a)
>   deriving (Eq, Ord, Show)

1. Write unfold for BinaryTree.

> unfold :: (a -> Maybe (a,b,a))
>        -> a
>        -> BinaryTree b
> unfold f a = go (f a)
>   where go Nothing            = Leaf
>         go (Just (a1, b, a2)) =
>           Node (unfold f a1)
>                b
>                (unfold f a2)

2. Make a tree builder.
Using the unfold function you’ve made for BinaryTree, write the following function:

> treeBuild :: Integer -> BinaryTree Integer
> treeBuild n = unfold build 0
>   where
>     build m
>       | m == n    = Nothing
>       | otherwise = Just (m+1, m, m+1)

You should be producing results that look like the following:

Prelude> treeBuild 0
Leaf
Prelude> treeBuild 1
Node Leaf 0 Leaf
Prelude> treeBuild 2
Node (Node Leaf 1 Leaf)
0
     (Node Leaf 1 Leaf)
Prelude> treeBuild 3
Node (Node (Node Leaf 2 Leaf)
           1
           (Node Leaf 2 Leaf))
     0
     (Node (Node Leaf 2 Leaf)
           1
           (Node Leaf 2 Leaf))
