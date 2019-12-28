Chapter 20

> module Foldables where
>
> import Data.Monoid
> import Test.QuickCheck
> import Test.QuickCheck.Checkers
> import Test.QuickCheck.Classes

Exercises: Library Functions

Implement the functions in terms of foldMap or foldr from Foldable, then try them out with multiple types that have Foldable instances.
1. This and the next one are nicer with foldMap, but foldr is fine too.

> sum :: (Foldable t, Num a) => t a -> a
> sum = getSum . Prelude.foldMap Sum

2.

> product :: (Foldable t, Num a) => t a -> a
> product = getProduct . Prelude.foldMap Product

3.

> elem :: (Foldable t, Eq a)
>      => a -> t a -> Bool
> elem x = getAny . Prelude.foldMap (Any . (== x))

4.

> minimum :: (Foldable t, Ord a)
>         => t a -> Maybe a
> minimum = foldr cmp Nothing
>   where
>     cmp a Nothing = Just a
>     cmp a b       = min (Just a) b

5.

> maximum :: (Foldable t, Ord a)
>         => t a -> Maybe a
> maximum = foldr cmp Nothing
>   where
>     cmp a Nothing = Just a
>     cmp a b       = max (Just a) b

6.

> null :: (Foldable t) => t a -> Bool
> null = not
>      . getAny
>      . Prelude.foldMap (Any . const True)

7.

> length :: (Foldable t) => t a -> Int
> length = getSum . Prelude.foldMap (Sum . const 1)

8. Some say this is all Foldable amounts to.

> toList :: (Foldable t) => t a -> [a]
> toList = Prelude.foldMap (:[])

9. Hint: use foldMap.

> fold :: (Foldable t, Monoid m) => t m -> m
> fold = Prelude.foldMap id

10. Define foldMap in terms of foldr.

> foldMap :: (Foldable t, Monoid m)
>         => (a -> m) -> t a -> m
> foldMap m =
>   foldr (\a acc ->
>           m a `mappend` acc)
>         mempty

Chapter Exercises

Write Foldable instances for the following datatypes.

1.

> data Constant a b =
>   Constant b
>   deriving (Eq, Show)

> instance Foldable (Constant a) where
>   foldMap fromB (Constant b) = fromB b

> instance Arbitrary b
>       => Arbitrary (Constant a b) where
>   arbitrary = Constant <$> arbitrary

> testConstantFoldable :: IO ()
> testConstantFoldable = testFold trigger
>   where trigger :: Constant String
>           (Int, String, String, Int, Int)
>         trigger = undefined

> testFold :: (Foldable f,
>              Arbitrary (f Int),
>              Arbitrary (f String),
>              Show (f Int), Show (f String))
>          => f (Int, String, String, Int, Int)
>          -> IO ()
> testFold f = quickBatch $ foldable f

2.

> data Two a b =
>   Two a b
>   deriving (Eq, Show)

> instance Foldable (Two a) where
>   foldMap f (Two a b) = f b

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Two a b) where
>   arbitrary = Two <$> arbitrary <*> arbitrary

> testTwoFoldable :: IO ()
> testTwoFoldable = testFold trigger
>   where
>     trigger = Two (2::Int) (2, "h", "i", 2, 2)

3.

> data Three a b c =
>   Three a b c
>   deriving (Eq, Show)

> instance Foldable (Three a b) where
>   foldMap f (Three a b c) = f c

> instance (Arbitrary a, Arbitrary b,
>           Arbitrary c)
>       => Arbitrary (Three a b c) where
>   arbitrary = Three
>           <$> arbitrary
>           <*> arbitrary
>           <*> arbitrary

> testThreeFoldable :: IO ()
> testThreeFoldable = testFold trigger
>   where
>     trigger =
>       Three (2::Int) "hello" (2, "h", "i", 2, 2)

4.

> data Three' a b =
>   Three' a b b
>   deriving (Eq, Show)

> instance Foldable (Three' a) where
>   foldMap f (Three' a x y) = f x

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Three' a b) where
>   arbitrary = Three' <$> arbitrary
>                      <*> arbitrary
>                      <*> arbitrary

> testThreePrimeFoldable :: IO ()
> testThreePrimeFoldable = testFold trigger
>   where
>     trigger =
>       Three' (3::Int)
>              (3, "h", "i", 3, 3)
>              (6, "l", "o", 6, 6)

5.

> data Four' a b =
>   Four' a b b b
>   deriving (Eq, Show)

> instance Foldable (Four' a) where
>   foldMap f (Four' a x y z) = f z

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Four' a b) where
>   arbitrary = Four'
>           <$> arbitrary
>           <*> arbitrary
>           <*> arbitrary
>           <*> arbitrary

> testFourPrimeFoldable :: IO ()
> testFourPrimeFoldable = testFold trigger
>   where
>     trigger =
>       Four' (4::Int)
>             (5, "h", "i", 5, 5)
>             (6, "l", "o", 6, 6)
>             (7, "m", "e", 7, 7)

Thinking cap time. Write a filter function for Foldable types using foldMap.

> filterF :: ( Applicative f
>            , Foldable t
>            , Monoid (f a))
>         => (a -> Bool) -> t a -> f a
> filterF p = Foldables.foldMap (\a ->
>   if p a
>   then pure a
>   else mempty )
