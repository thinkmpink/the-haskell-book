Chapter 17. Applicative

> module Applicatives where
>
> import Control.Applicative (liftA2, liftA3)
> import Data.List (elemIndex)
> import Data.Monoid (First)
> import Test.QuickCheck
> import Test.QuickCheck.Checkers
> import Test.QuickCheck.Classes
> import Test.Hspec

Exercises: Lookups
In the following exercises you will need to use the following terms to make the expressions typecheck:

1. pure
2. (<$>)
3. (<*>)
Make the following expressions typecheck.

1.

> added :: Maybe Integer
> added =
>   (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

2.

> y :: Maybe Integer
> y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

> z :: Maybe Integer
> z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

> tupled :: Maybe (Integer, Integer)
> tupled = (,) <$> y <*> z

3.

> x :: Maybe Int
> x = elemIndex 3 [1, 2, 3, 4, 5]

> y' :: Maybe Int
> y' = elemIndex 4 [1, 2, 3, 4, 5]

> max' :: Int -> Int -> Int
> max' = max

> maxed :: Maybe Int
> maxed = max' <$> x <*> y'

4.

> xs = [1, 2, 3]
> ys = [4, 5, 6]

> x'' :: Maybe Integer
> x'' = lookup 3 $ zip xs ys

> y'' :: Maybe Integer
> y'' = lookup 2 $ zip xs ys

> summed :: Maybe Integer
> summed = fmap sum $ (,) <$> x'' <*> y''

Exercise: Identity Instance
Write an Applicative instance for Identity.

> newtype Identity a = Identity a
>    deriving (Eq, Ord, Show)
>
> instance Functor Identity where
>   fmap f (Identity a) = Identity (f a)
>
> instance Applicative Identity where
>   pure a               = Identity a
>   (<*>) (Identity f) a = f <$> a

Exercise: Constant Instance
Write an Applicative instance for Constant.

> newtype Constant a b =
>   Constant { getConstant :: a }
>   deriving (Eq, Ord, Show)

> instance Functor (Constant a) where
>   fmap _ (Constant a) = Constant a

> instance Monoid a
>       => Applicative (Constant a) where
>   pure _ = Constant mempty
>   Constant a <*> Constant b = Constant $ mappend a b

Exercise: Fixer Upper
Given the function and values provided, use (<$>) from Functor, (<*>) and pure from the Applicative type class to fill in missing bits of the broken code to make it work.

1. const <$> Just "Hello" <*> "World"

Prelude> const <$> Just "Hello" <*> pure "World"

Just "Hello"

2. (,,,) Just 90
<*> Just 10 Just "Tierness" [1, 2, 3]

Prelude> (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

Just (90,10,"Tierness",[1,2,3])


List Applicative Exercise
Implement the list Applicative. Writing a minimally complete Applicative instance calls for writing the definitions of both pure and <*>. We’re going to provide a hint as well. Use the checkers library to validate your Applicative instance.

> data List a =
>     Nil
>   | Cons a (List a)
>   deriving (Eq, Show)

> append :: List a -> List a -> List a
> append Nil ys = ys
> append (Cons x xs) ys =
>   Cons x $ xs `append` ys

> fold :: (a -> b -> b) -> b -> List a -> b
> fold _ b Nil         = b
> fold f b (Cons x xs) = f x (fold f b xs)

> concat' :: List (List a) -> List a
> concat' = fold append Nil

> -- write this one in terms of concat' and fmap
> flatMap :: (a -> List b)
>         -> List a
>         -> List b
> flatMap f = concat' . fmap f

> instance Functor List where
>   fmap _ Nil = Nil
>   fmap f (Cons a l) = Cons (f a) (f <$> l)

> instance Applicative List where
>   pure a = Cons a Nil
>
>   Nil <*> _   = Nil
>   _   <*> Nil = Nil
>   Cons f fs <*> xs = append (f <$> xs) (fs <*> xs)

> instance Arbitrary a
>       => Arbitrary (List a) where
>   arbitrary = frequency
>     [ (4, Cons <$> arbitrary <*> arbitrary)
>     , (1, return Nil) ]

> instance Eq a
>       => EqProp (List a) where
>   (=-=) = eq

> testListApplicative :: IO ()
> testListApplicative = do
>   let trigger = undefined
>         :: List (String, String, Int)
>   quickBatch (applicative trigger)


ZipList Applicative Exercise
Implement the ZipList Applicative. Use the checkers library to validate your Applicative instance. We’re going to provide the EqProp instance and explain the weirdness in a moment.

> newtype ZipList' a =
>   ZipList' [a]
>   deriving (Eq, Show)
>
> instance Eq a => EqProp (ZipList' a) where
>   xs =-= ys = xs' `eq` ys'
>     where xs' = let (ZipList' l) = xs
>                 in take 3000 l
>           ys' = let (ZipList' l) = ys
>                 in take 3000 l
>
> instance Arbitrary a
>       => Arbitrary (ZipList' a) where
>   arbitrary = ZipList' <$> arbitrary
>
> instance Functor ZipList' where
>   fmap f (ZipList' xs) =
>     ZipList' $ fmap f xs
>
> instance Applicative ZipList' where
>   pure a = ZipList' $ repeat a
>   ZipList' fs <*> ZipList' xs =
>     ZipList' $ zipWith ($) fs xs

> testZipListApplicative :: IO ()
> testZipListApplicative = do
>   quickBatch $ applicative
>     (undefined :: ZipList' (String, String, Int))


Exercise: Variations on Either
Validation has the same representation as Either, but it can be different. The Functor will behave the same, but the Applicative will be different. See above for an idea of how Validation should behave. Use the checkers library.

> data Validation e a =
>     Failure' e
>   | Success' a deriving (Eq, Show)
>
> -- same as Either
> instance Functor (Validation e) where
>   fmap _ (Failure' e) = (Failure' e)
>   fmap f (Success' a) = Success' $ f a
>
> -- This is different
> instance Monoid e =>
>          Applicative (Validation e) where
>   pure = Success'
>
>   Failure' a <*> Failure' b =
>     Failure' $ mappend a b
>   Failure' a <*> _          = Failure' a
>   _          <*> Failure' b = Failure' b
>   Success' a <*> Success' b = Success' $ a b

> instance (Arbitrary e, Arbitrary a)
>       => Arbitrary (Validation e a) where
>   arbitrary = oneof
>     [ Failure' <$> arbitrary
>     , Success' <$> arbitrary ]

> instance (Eq e, Eq a)
>       => EqProp (Validation e a) where
>   (=-=) = eq

> testValidationApplicative :: IO ()
> testValidationApplicative = do
>   quickBatch $ applicative
>     (undefined
>      :: Validation String (String, String, Int))


Chapter Exercises.

Given a type that has an instance of Applicative, specialize the types of the methods. Test your specialization in the REPL. One way to do this is to bind aliases of the type class methods to more concrete types that have the type we told you to fill in.

1. -- Type
   []

   -- Methods
   pure ::a -> ?a
   (<*>) :: ? (a -> b) -> ? a -> ? b

> lpure :: a -> [a]
> lpure = pure

> lApply :: [a -> b] -> [a] -> [b]
> lApply = (<*>)

*Applicatives> pure (*2) <*> pure (1 :: Int)
2
*Applicatives> pure (*2) <*> pure 1 :: [Int]
[2]
*Applicatives> lpure (*2) <*> lpure 1 :: [Int]
[2]
*Applicatives> lApply [(*2)] (lpure 1)

<interactive>:50:1: warning: [-Wtype-defaults]
    • Defaulting the following constraints to type ‘Integer’
        (Show b0) arising from a use of ‘print’ at <interactive>:50:1-23
        (Num b0) arising from a use of ‘it’ at <interactive>:50:1-23
    • In a stmt of an interactive GHCi command: print it
[2]
*Applicatives> lApply [(*2)] (lpure 1) :: [Integer]
[2]

2. IO

> ioPure :: a -> IO a
> ioPure = pure

> ioApply :: IO (a -> b) -> IO a -> IO b
> ioApply = (<*>)

*Applicatives> ioPure "hello"
"hello"
*Applicatives> ioPure (5 :: Int)
5
*Applicatives> ioPure (+5) <*> ioPure (5 :: Int)
10
*Applicatives> ioApply (pure (+5)) (ioPure (5 :: Int))
10

3. (,) a

> tuplePure :: Monoid a => b -> (a, b)
> tuplePure = pure

> tupleApply :: Monoid a
>            => (a, b -> c)
>            -> (a, b)
>            -> (a, c)
> tupleApply = (<*>)

*Applicatives> tuplePure 5 :: Num a => (String, a)

<interactive>:64:1: warning: [-Wtype-defaults]
    • Defaulting the following constraints to type ‘Integer’
        (Show a0) arising from a use of ‘print’ at <interactive>:64:1-35
        (Num a0) arising from a use of ‘it’ at <interactive>:64:1-35
    • In a stmt of an interactive GHCi command: print it
("",5)
*Applicatives> tuplePure (+5) `tupleApply` ("hello", 2)

<interactive>:65:1: warning: [-Wtype-defaults]
    • Defaulting the following constraints to type ‘Integer’
        (Show c0) arising from a use of ‘print’ at <interactive>:65:1-40
        (Num c0) arising from a use of ‘it’ at <interactive>:65:1-40
    • In a stmt of an interactive GHCi command: print it
("hello",7)
*Applicatives> tuplePure (+5) <*> ("hello", 2 :: Int)
("hello",7)

4. (->) e

> funcPure :: a -> e -> a
> funcPure = pure

> funcApply :: (e -> a -> b)
>           -> (e -> a)
>           -> (e -> b)
> funcApply = (<*>)

*Applicatives> funcPure (5 :: Int) (2 :: Int)
5
*Applicatives> (flip take) `funcApply` funcPure (5 :: Int) $ "hellogooodbye"
"hello"
*Applicatives> (flip take) <*> funcPure (5 :: Int) $ "hellogooodbye"
"hello"


Write instances for the following datatypes. Confused? Write out what the type should be. Use the checkers library to validate the instances.
1.

> data Pair a = Pair a a
>   deriving (Eq, Show)

> instance Functor Pair where
>   fmap f (Pair a b) = Pair (f a) (f b)

> instance Applicative Pair where
>   pure a = Pair a a
>   Pair f g <*> Pair x y =
>     Pair (f x) (g y)

> instance Arbitrary a
>       => Arbitrary (Pair a) where
>   arbitrary = Pair <$> arbitrary <*> arbitrary

> instance Eq a
>       => EqProp (Pair a) where
>   (=-=) = eq

> testPairApplicative :: IO ()
> testPairApplicative =
>   quickBatch $ applicative (undefined
>     :: Pair (String, String, Int))

2. This should look familiar.

> data Two a b = Two a b
>   deriving (Eq, Show)

> instance Functor (Two a) where
>   fmap f (Two a b) = Two a $ f b

> instance Monoid a
>       => Applicative (Two a) where
>   pure b = Two mempty b
>   Two f g <*> Two x y = Two (mappend f x) (g y)

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Two a b) where
>   arbitrary = Two <$> arbitrary <*> arbitrary

> instance (Eq a, Eq b)
>       => EqProp (Two a b) where
>   (=-=) = eq

> testTwoApplicative :: IO ()
> testTwoApplicative = quickBatch $ applicative
>   (undefined :: Two String (String, Int, Int))

3.

> data Three a b c = Three a b c
>   deriving (Eq, Show)

> instance Functor (Three a b) where
>   fmap f (Three a b c) = Three a b $ f c

> instance (Monoid a, Monoid b)
>       => Applicative (Three a b) where
>   pure c = Three mempty mempty c
>   Three f g h <*> Three x y z =
>     Three (mappend f x)
>           (mappend g y)
>           (h z)

> instance (Arbitrary a, Arbitrary b,
>           Arbitrary c)
>       => Arbitrary (Three a b c) where
>   arbitrary =
>     liftA3 Three arbitrary arbitrary arbitrary

> instance (Eq a, Eq b, Eq c)
>       => EqProp (Three a b c) where
>   (=-=) = eq

> testThreeApplicative :: IO ()
> testThreeApplicative = quickBatch $
>   applicative (undefined
>     :: Three String
>              (First Int)
>              (Int, Int, String))

4.

> data Three' a b = Three' a b b
>   deriving (Eq, Show)

> instance Functor (Three' a) where
>   fmap f (Three' a x y) =
>     Three' a (f x) (f y)

> instance Monoid a
>       => Applicative (Three' a) where
>   pure b = Three' mempty b b
>   Three' f m n <*> Three' a x y =
>     Three' (mappend f a) (m x) (n y)

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Three' a b) where
>   arbitrary = liftA3 Three'
>     arbitrary arbitrary arbitrary

> instance (Eq a, Eq b)
>       => EqProp (Three' a b) where
>   (=-=) = eq

> testThreePrimeApplicative :: IO ()
> testThreePrimeApplicative = do
>   quickBatch $ applicative (undefined
>     :: Three' String (Int, Int, String))

5.

> data Four a b c d = Four a b c d
>   deriving (Eq, Show)

> instance Functor (Four a b c) where
>   fmap f (Four a b c d) =
>     Four a b c $ f d

> instance (Monoid a, Monoid b, Monoid c)
>       => Applicative (Four a b c) where
>   pure a = Four mempty mempty mempty a
>   (<*>) (Four f g h k)
>         (Four w x y z) =
>     Four (mappend f w)
>          (mappend g x)
>          (mappend h y)
>          (k z)

> instance (Arbitrary a, Arbitrary b,
>           Arbitrary c, Arbitrary d)
>       => Arbitrary (Four a b c d) where
>   arbitrary = Four <$> arbitrary
>                    <*> arbitrary
>                    <*> arbitrary
>                    <*> arbitrary

> instance (Eq a, Eq b, Eq c, Eq d)
>       => EqProp (Four a b c d) where
>   (=-=) = eq

> testFourApplicative :: IO ()
> testFourApplicative = do
>   quickBatch $ applicative (undefined
>     :: Four (First Int)
>             (First Bool)
>             String
>             (Int, String, Int))

6.

> data Four' a b = Four' a a a b
>   deriving (Eq, Show)

> instance Functor (Four' a) where
>   fmap f (Four' a b c x) =
>     Four' a b c $ f x

> instance Monoid a
>       => Applicative (Four' a) where
>   pure a = Four' mempty mempty mempty a
>   (<*>) (Four' f g h k)
>         (Four' w x y z) =
>     Four' (mappend f w)
>           (mappend g x)
>           (mappend h y)
>           (k z)

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Four' a b) where
>   arbitrary = Four'
>           <$> arbitrary
>           <*> arbitrary
>           <*> arbitrary
>           <*> arbitrary

> instance (Eq a, Eq b)
>       => EqProp (Four' a b) where
>   (=-=) = eq

> testFourPrimeApplicative :: IO ()
> testFourPrimeApplicative = do
>   quickBatch $ applicative (undefined
>     :: Four' String (Int, Int, String))

Combinations
Remember the vowels and stops exercise in the folds chapter? Write the function to generate the possible combinations of three input lists using liftA3 from Control.Applicative.

> stops :: String
> stops = "pbtdkg"

> vowels :: String
> vowels = "aeiou"

> combos :: [a] -> [b] -> [c] -> [(a, b, c)]
> combos = liftA3 (,,)
