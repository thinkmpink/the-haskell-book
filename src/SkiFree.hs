{-# LANGUAGE FlexibleContexts #-}

-- From Chapter 21. Traversable
-- Chapter Exercises

module SkiFree where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- S Maybe Int = S (Maybe Int) Int
data S n a = S (n a) a
  deriving (Eq, Show)

instance Functor n
      => Functor (S n) where
  fmap f (S n a) = S (f <$> n) (f a)

instance Applicative n
      => Applicative (S n) where
  pure a = S (pure a) a

  -- g  :: a -> b
  -- nf :: n (a -> b)
  -- a  :: n a
  -- b  :: a
  S nf g <*> S a b =
    S (nf <*> a) (g b)

instance Foldable n
      => Foldable (S n) where
  foldMap f (S n a) =
    foldMap f n <> f a


instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
        => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary

instance ( Applicative n
         , Testable (n Property)
         , Eq a
         , Eq (n a) , EqProp a)
         => EqProp (S n a) where
  (=-=) = eq

instance Traversable n
      => Traversable (S n) where
  traverse f (S n a) =
    S <$> traverse f n <*> f a

main =
  sample' (arbitrary :: Gen (S [] Int))

type TS = S []

testSTraversable :: IO ()
testSTraversable = do
  let trigger :: TS (Int, Int, [Int])
      trigger = undefined
      trigger2 :: TS (Int, Int, [Int], Int, Int)
      trigger2 = undefined
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (foldable trigger2)
  quickBatch (traversable trigger)
