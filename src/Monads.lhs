Chapter 18. Monad

> module Monads where
>
> import Control.Monad
> import Test.QuickCheck
> import Test.QuickCheck.Checkers
> import Test.QuickCheck.Classes

Exercise. Write bind in terms of fmap and join.

> bind :: Monad m => (a -> m b) -> m a -> m b
> bind f = join . fmap f

> smallMultiples :: [Integer] -> [Integer]
> smallMultiples xs = do
>   x <- xs
>   if x `mod` 2 == 0
>   then [x, x*x, x*x]
>   else if x `mod` 3 == 0
>        then [x, x*x, x*x, x*x]
>        else [x]

Short Exercise: Either Monad
Implement the Either monad.

> data Sum a b =
>     First a
>   | Second b
>   deriving (Eq, Show)

> instance Functor (Sum a) where
>   fmap f (First a) = First a
>   fmap f (Second b) = Second $ f b

> instance Applicative (Sum a) where
>   pure = Second
>   First f <*> _       = First f
>   _       <*> First f = First f
>   Second g <*> Second x =
>     Second (g x)

> instance Monad (Sum a) where
>   First a  >>= _ = First a
>   Second b >>= f = f b

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Sum a b) where
>   arbitrary = oneof
>     [ First <$> arbitrary
>     , Second <$> arbitrary ]

> instance (Eq a, Eq b)
>       => EqProp (Sum a b) where
>   (=-=) = eq

> testSumMonad :: IO ()
> testSumMonad =
>   let trigger :: Sum Int (String, Int, Int)
>       trigger = undefined
>   in do
>     quickBatch $ functor trigger
>     quickBatch $ applicative trigger
>     quickBatch $ monad trigger

> -- flipped kleisli composition: use >=>
> mcomp :: Monad m
>       => (b -> m c)
>       -> (a -> m b)
>       -> a -> m c
> mcomp f g a =
>   g a >>= f


Chapter Exercises

Write Monad instances for the following types. Use the QuickCheck properties we showed you to validate your instances.

1. Welcome to the Nope Monad, where nothing happens and nobody cares.

> data Nope a =
>   NopeDotJpg
>   deriving (Eq, Show)
>
>   -- We're serious. Write it anyway.

> instance Functor Nope where
>   fmap _ NopeDotJpg = NopeDotJpg

> instance Applicative Nope where
>   pure _ = NopeDotJpg
>   _ <*> _ = NopeDotJpg

> instance Monad Nope where
>   NopeDotJpg >>= _ = NopeDotJpg

> instance Arbitrary (Nope a) where
>   arbitrary = return NopeDotJpg

> instance EqProp (Nope a) where
>   (=-=) = eq

> testNopeMonad :: IO ()
> testNopeMonad = do
>   let trigger :: Nope (Int, Int, Int)
>       trigger = undefined
>   quickBatch $ functor trigger
>   quickBatch $ applicative trigger
>   quickBatch $ monad trigger

2.

> data BahEither b a =
>     PLeft a
>   | PRight b
>   deriving (Eq, Show)

> instance Functor (BahEither b) where
>   fmap f (PLeft a) = PLeft $ f a
>   fmap _ (PRight b) = PRight b

> instance Applicative (BahEither b) where
>   pure = PLeft
>   PRight b <*>        _ = PRight b
>   _        <*> PRight b = PRight b
>   PLeft a  <*> PLeft  b = PLeft $ a b

> instance Monad (BahEither b) where
>   PRight b >>= _ = PRight b
>   PLeft a >>= f  = f a

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (BahEither b a) where
>   arbitrary = oneof
>     [ PRight <$> arbitrary
>     , PLeft <$> arbitrary ]

> instance (Eq a, Eq b)
>       => EqProp (BahEither b a) where
>   (=-=) = eq

> testBahEitherMonad :: IO ()
> testBahEitherMonad = do
>   let trigger :: BahEither Int (String, Int, Int)
>       trigger = undefined
>   quickBatch $ functor trigger
>   quickBatch $ applicative trigger
>   quickBatch $ monad trigger

3. Write a Monad instance for Identity.

> newtype Identity a = Identity a
>   deriving (Eq, Ord, Show)

> instance Functor Identity where
>   fmap f (Identity a) = Identity $ f a

> instance Applicative Identity where
>   pure = Identity
>   Identity f <*> Identity a = Identity $ f a

> instance Monad Identity where
>   Identity a >>= f = f a

> instance Arbitrary a
>       => Arbitrary (Identity a) where
>  arbitrary = Identity <$> arbitrary

> instance Eq a
>       => EqProp (Identity a) where
>   (=-=) = eq

> testIdentityMonad :: IO ()
> testIdentityMonad = do
>   let trigger :: Identity (Int, Int, String)
>       trigger = undefined
>   quickBatch $ functor trigger
>   quickBatch $ applicative trigger
>   quickBatch $ monad trigger

4. This one should be easier than the Applicative instance was. Remember to use the Functor that Monad requires, then see where the chips fall.

> data List a =
>     Nil
>   | Cons a (List a)
>   deriving (Eq, Show)

> instance Functor List where
>   fmap _ Nil = Nil
>   fmap f (Cons a l) = Cons (f a) (f <$> l)

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


> instance Applicative List where
>   pure a = Cons a Nil
>
>   Nil <*> _   = Nil
>   _   <*> Nil = Nil
>   Cons f fs <*> xs = append (f <$> xs) (fs <*> xs)

> instance Semigroup (List a) where
>   (<>) = append

> instance Monad List where
>   l >>= f = concat' $ fmap f l

> instance Arbitrary a
>       => Arbitrary (List a) where
>   arbitrary = frequency
>     [ (4, Cons <$> arbitrary <*> arbitrary)
>     , (1, return Nil) ]

> instance Eq a
>       => EqProp (List a) where
>   (=-=) = eq

> testListMonad :: IO ()
> testListMonad = do
>   let trigger :: List (Int, Int, Int)
>       trigger = undefined
>   quickBatch $ functor trigger
>   quickBatch $ applicative trigger
>   quickBatch $ monad trigger

Write the following functions using the methods provided by Monad and Functor. Using stuff like identity and composition is fine, but it has to typecheck with types provided.

1.

> j :: Monad m => m (m a) -> m a
> j = flip (>>=) id

Expecting the following behavior:
     Prelude> j [[1, 2], [], [3]]
     [1,2,3]
     Prelude> j (Just (Just 1))
     Just 1
     Prelude> j (Just Nothing)
     Nothing
     Prelude> j Nothing
     Nothing

2.

> l1 :: Monad m => (a -> b) -> m a -> m b
> l1 = fmap

3.

> l2 :: Monad m
>    => (a -> b -> c) -> m a -> m b -> m c
> l2 f a b = f `fmap` a `ap` b

4.

> a :: Monad m
>   => m a -> m (a -> b) -> m b
> a = flip (ap)

5. You'll need recursion for this one.

> meh :: Monad m
>     => [a] -> (a -> m b) -> m [b]
> meh []     _ = return []
> meh (a:as) f = do
>   b <- f a
>   bs <- meh as f
>   return (b:bs)

6. Hint: reuse "meh"

> flipType :: Monad m => [m a] -> m [a]
> flipType = flip meh id
