Chapter 21. Traversable

> module Traversables where
>
> import Data.Monoid
> import Test.QuickCheck
> import Test.QuickCheck.Checkers
> import Test.QuickCheck.Classes

Chapter Exercises

Traversable instances
Write a Traversable instance for the datatype provided, filling in any required superclasses. Use QuickCheck to validate your instances.

Identity
Write a Traversable instance for Identity.

> newtype Identity a = Identity a
>   deriving (Eq, Show)

> instance Functor Identity where
>   fmap f (Identity a) = Identity $ f a

> instance Applicative Identity where
>   pure = Identity
>   Identity f <*> Identity a = Identity $ f a

> instance Arbitrary a
>       => Arbitrary (Identity a) where
>   arbitrary = Identity <$> arbitrary

> instance Foldable Identity where
>   foldMap f (Identity a) = f a

> instance Traversable Identity where
>   traverse f (Identity a) =
>     Identity <$> f a

> instance Eq a
>       => EqProp (Identity a) where
>   (=-=) = eq

> type TId = Identity

> testIdentityTraversable :: IO ()
> testIdentityTraversable = do
>   let trigger :: TId (Int, Int, [Int])
>       trigger = undefined
>       trigger2 :: TId (Int, Int, [Int], Int, Int)
>       trigger2 = undefined
>   quickBatch (functor trigger)
>   quickBatch (applicative trigger)
>   quickBatch (foldable trigger2)
>   quickBatch (traversable trigger)

Constant

> newtype Constant a b =
>   Constant { getConstant :: a }
>   deriving (Eq, Show)

> instance Functor (Constant a) where
>   fmap _ (Constant a) = Constant a

> instance Monoid a
>       => Applicative (Constant a) where
>   pure _ = Constant mempty
>   Constant f <*> Constant a = Constant $ f <> a

> instance Foldable (Constant a) where
>   foldMap _ (Constant _) = mempty

> instance Traversable (Constant a) where
>   traverse _ (Constant a) = pure $ Constant a

> instance Arbitrary a
>       => Arbitrary (Constant a b) where
>   arbitrary = Constant <$> arbitrary

> instance Eq a
>       => EqProp (Constant a b) where
>   (=-=) = eq

> type TCo = Constant (Sum Int)

> testConstantTraversable :: IO ()
> testConstantTraversable = do
>   let trigger :: TCo (Int, Int, [Int])
>       trigger = undefined
>       trigger2 :: TCo (Int, Int, [Int], Int, Int)
>       trigger2 = undefined
>   quickBatch (functor trigger)
>   quickBatch (applicative trigger)
>   quickBatch (foldable trigger2)
>   quickBatch (traversable trigger)

Maybe

> data Optional a =
>     Nada
>   | Yep a
>   deriving (Eq, Show)

> instance Functor Optional where
>   fmap _ Nada = Nada
>   fmap f (Yep a) = Yep $ f a

> instance Applicative Optional where
>   pure = Yep
>
>   Nada  <*> _     = Nada
>   _     <*> Nada  = Nada
>   Yep f <*> Yep a = Yep $ f a

> instance Foldable Optional where
>   foldMap _ Nada    = mempty
>   foldMap f (Yep a) = f a

> instance Traversable Optional where
>   traverse _ Nada    = pure Nada
>   traverse f (Yep a) = Yep <$> f a

> instance Arbitrary a
>       => Arbitrary (Optional a) where
>   arbitrary =
>     frequency [ (1, pure Nada)
>               , (3, Yep <$> arbitrary)
>               ]

> instance Eq a
>       => EqProp (Optional a) where
>   (=-=) = eq

> type TOp = Optional

> testOptionalTraversable :: IO ()
> testOptionalTraversable = do
>   let trigger :: TOp (Int, Int, [Int])
>       trigger = undefined
>       trigger2 :: TOp (Int, Int, [Int], Int, Int)
>       trigger2 = undefined
>   quickBatch (functor trigger)
>   quickBatch (applicative trigger)
>   quickBatch (foldable trigger2)
>   quickBatch (traversable trigger)

List

> data List a =
>     Nil
>   | Cons a (List a)
>   deriving (Eq, Show)

> instance Functor List where
>   fmap _ Nil         = Nil
>   fmap f (Cons a as) = f a `Cons` fmap f as

> instance Semigroup (List a) where
>   Nil <> a   = a
>   a   <> Nil = a
>   Cons a as <> bs = Cons a (as <> bs)

> instance Monoid (List a) where
>   mempty = Nil

> instance Applicative List where
>   pure a = Cons a Nil
>
>   Nil       <*> _         = Nil
>   _         <*> Nil       = Nil
>   Cons f fs <*> Cons a as =
>        (f <$> Cons a as)
>     <> (fs <*> Cons a as)

> instance Foldable List where
>   foldMap _ Nil         = mempty
>   foldMap f (Cons a as) =
>     f a <> foldMap f as

> instance Traversable List where
>   traverse _ Nil         = pure Nil
>   traverse f (Cons a as) =
>     Cons <$> f a <*> traverse f as

> instance Arbitrary a
>       => Arbitrary (List a) where
>   arbitrary =
>     frequency
>       [ (1, pure Nil)
>       , (4, Cons <$> arbitrary <*> arbitrary)
>       ]

> instance Eq a
>       => EqProp (List a) where
>   (=-=) = eq

> type TLi = List

> testListTraversable :: IO ()
> testListTraversable = do
>   let trigger :: TLi (Int, Int, [Int])
>       trigger = undefined
>       trigger2 :: TLi (Int, Int, [Int], Int, Int)
>       trigger2 = undefined
>   quickBatch (functor trigger)
>   quickBatch (applicative trigger)
>   quickBatch (foldable trigger2)
>   quickBatch (traversable trigger)

Three

> data Three a b c =
>   Three a b c
>   deriving (Eq, Show)

> instance Functor (Three a b) where
>   fmap f (Three a b c) = Three a b $ f c

> instance (Monoid a, Monoid b)
>       => Applicative (Three a b) where
>   pure = Three mempty mempty
>   Three a b f <*> Three x y z =
>     Three (a <> x) (b <> y) $ f z

> instance Foldable (Three a b) where
>   foldMap f (Three _ _ c) = f c

> instance Traversable (Three a b) where
>   traverse f (Three a b c) =
>     Three a b <$> f c

> instance (Arbitrary a, Arbitrary b,
>           Arbitrary c)
>       => Arbitrary (Three a b c) where
>   arbitrary = Three <$> arbitrary
>                     <*> arbitrary
>                     <*> arbitrary

> instance (Eq a, Eq b, Eq c)
>       => EqProp (Three a b c) where
>   (=-=) = eq

> type T3 = Three (Sum Int) (Product Int)

> testThreeTraversable :: IO ()
> testThreeTraversable = do
>   let trigger :: T3 (Int, Int, [Int])
>       trigger = undefined
>       trigger2 :: T3 (Int, Int, [Int], Int, Int)
>       trigger2 = undefined
>   quickBatch (functor trigger)
>   quickBatch (applicative trigger)
>   quickBatch (foldable trigger2)
>   quickBatch (traversable trigger)

Pair

> data Pair a b =
>   Pair a b
>   deriving (Eq, Show)

> instance Functor (Pair a) where
>   fmap f (Pair a b) = Pair a $ f b

> instance Monoid a
>       => Applicative (Pair a) where
>   pure = Pair mempty
>   Pair a f <*> Pair b c =
>     Pair (a <> b) $ f c

> instance Foldable (Pair a) where
>   foldMap f (Pair _ b) = f b

> instance Traversable (Pair a) where
>   traverse f (Pair a b) =
>     Pair a <$> f b

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Pair a b) where
>   arbitrary = Pair <$> arbitrary
>                    <*> arbitrary

> instance (Eq a, Eq b)
>       => EqProp (Pair a b) where
>   (=-=) = eq

> type TPr = Pair [Int]

> testPairTraversable :: IO ()
> testPairTraversable = do
>   let trigger :: TPr (Int, Int, [Int])
>       trigger = undefined
>       trigger2 :: TPr (Int, Int, [Int], Int, Int)
>       trigger2 = undefined
>   quickBatch (functor trigger)
>   quickBatch (applicative trigger)
>   quickBatch (foldable trigger2)
>   quickBatch (traversable trigger)

Big
When you have more than one value of type 𝑏, you’ll want to use Monoid and Applicative for the Foldable and Traversable instances respectively.

> data Big a b =
>   Big a b b
>   deriving (Eq, Show)

> instance Functor (Big a) where
>   fmap f (Big a m n) =
>     Big a (f m) (f n)

> instance Monoid a
>       => Applicative (Big a) where
>   pure b = Big mempty b b
>   Big a f g <*> Big x y z =
>     Big (a <> x) (f y) (g z)

> instance Foldable (Big a) where
>   foldMap f (Big _ m n) =
>     f m <> f n

> instance Traversable (Big a) where
>   traverse f (Big a m n) =
>     Big a <$> f m <*> f n

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Big a b) where
>   arbitrary =
>     Big <$> arbitrary
>         <*> arbitrary
>         <*> arbitrary

> instance (Eq a, Eq b)
>       => EqProp (Big a b) where
>   (=-=) = eq

> type TBg = Big (Product Int)

> testBigTraversable :: IO ()
> testBigTraversable = do
>   let trigger :: TBg (Int, Int, [Int])
>       trigger = undefined
>       trigger2 :: TBg (Int, Int, [Int], Int, Int)
>       trigger2 = undefined
>   quickBatch (functor trigger)
>   quickBatch (applicative trigger)
>   quickBatch (foldable trigger2)
>   quickBatch (traversable trigger)

Bigger
Same as for Big.

> data Bigger a b =
>   Bigger a b b b
>   deriving (Eq, Show)

> instance Functor (Bigger a) where
>   fmap f (Bigger a x y z) =
>     Bigger a (f x) (f y) (f z)

> instance Monoid a
>       => Applicative (Bigger a) where
>   pure a = Bigger mempty a a a
>   Bigger a f g h <*> Bigger b x y z =
>     Bigger (a <> b) (f x) (g y) (h z)

> instance Foldable (Bigger a) where
>   foldMap f (Bigger _ x y z) =
>     f x <> f y <> f z

> instance Traversable (Bigger a) where
>   traverse f (Bigger a x y z) =
>     Bigger a <$> f x <*> f y <*> f z

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Bigger a b) where
>   arbitrary =
>     Bigger <$> arbitrary
>            <*> arbitrary
>            <*> arbitrary
>            <*> arbitrary

> instance (Eq a, Eq b)
>       => EqProp (Bigger a b) where
>   (=-=) = eq

> type TBgr = Bigger (Sum Int)

> testBiggerTraversable :: IO ()
> testBiggerTraversable = do
>   let trigger :: TBgr (Int, Int, [Int])
>       trigger = undefined
>       trigger2 :: TBgr (Int, Int, [Int], Int, Int)
>       trigger2 = undefined
>   quickBatch (functor trigger)
>   quickBatch (applicative trigger)
>   quickBatch (foldable trigger2)
>   quickBatch (traversable trigger)

S
This may be difficult. To make it easier, we’ll give you the constraints and QuickCheck instances:

see SkiFree module
