Chapter 25. Composing Types

> {-# LANGUAGE InstanceSigs #-}
>
> module ComposingTypes where
>
> import Test.Hspec
> import Test.QuickCheck
> import Test.QuickCheck.Checkers
> import Test.QuickCheck.Classes

Exercise. Twinplicative

> data Compose f g a =
>   Compose { getCompose :: f (g a) }
>   deriving (Eq, Show)

> instance (Functor f, Functor g)
>       => Functor (Compose f g) where
>   fmap f (Compose fga) =
>     Compose $ (fmap . fmap) f fga

> instance (Applicative f, Applicative g)
>       => Applicative (Compose f g) where
>   pure :: a -> Compose f g a
>   pure = Compose . pure . pure
>
>   (<*>) :: Compose f g (a -> b)
>         -> Compose f g a
>         -> Compose f g b
>   (Compose f) <*> (Compose a) =
>     Compose $ fmap (<*>) f <*> a

Exercises

1. Write  the Compose Foldable instance.

> instance (Foldable f, Foldable g)
>       => Foldable (Compose f g) where
>   foldMap :: Monoid m
>           => (a -> m)
>           -> Compose f g a
>           -> m
>   foldMap f (Compose fga) =
>     (foldMap . foldMap) f fga

> instance (Traversable f, Traversable g)
>       => Traversable (Compose f g) where
>   traverse :: (Applicative h)
>            => (a -> h b)
>            -> Compose f g a
>            -> h (Compose f g b)
>   traverse ahb (Compose fga) =
>     fmap Compose $
>       (traverse . traverse) ahb fga

> instance (Arbitrary (f (g a)))
>       => Arbitrary (Compose f g a) where
>   arbitrary = Compose <$> arbitrary

> instance Eq (f (g a))
>       => EqProp (Compose f g a) where
>   (=-=) = eq

> type TCo = Compose [] Maybe

> testComposeTraversable :: IO ()
> testComposeTraversable = do
>   quickBatch $ functor trigger
>   quickBatch $ applicative trigger
>   quickBatch $ foldable trigger2
>   quickBatch $ traversable trigger
>
>   where
>     trigger :: TCo (Int, Int, [Int])
>     trigger = undefined
>     trigger2 :: TCo (Int, Int, [Int], Int, Int)
>     trigger2 = undefined

Exercise. Bifunctors

> class Bifunctor p where
>   {-# MINIMAL bimap | first, second #-}
>
>   bimap :: (a -> b)
>         -> (c -> d)
>         -> p a c
>         -> p b d
>   bimap f g = first f . second g
>
>   first :: (a -> b) -> p a c -> p b c
>   first f = bimap f id
>
>   second :: (b -> c) -> p a b -> p a c
>   second = bimap id

1.

> data Deux a b = Deux a b

> instance Bifunctor Deux where
>   bimap fa fb (Deux a b) =
>     Deux (fa a) (fb b)

2.

> data Const a b = Const a

> instance Bifunctor Const where
>   bimap fa _ (Const a) =
>     Const (fa a)

3.

> data Drei a b c = Drei a b c

> instance Bifunctor (Drei a) where
>   bimap fb fc (Drei a b c) =
>     Drei a (fb b) (fc c)

4.

> data SuperDrei a b c = SuperDrei a b

> instance Bifunctor (SuperDrei a) where
>   bimap fb _ (SuperDrei a b) =
>     SuperDrei a (fb b)

5.

> data SemiDrei a b c = SemiDrei a

> instance Bifunctor (SemiDrei a) where
>   bimap _ _ (SemiDrei a) =
>     SemiDrei a

6.

> data Quadriceps a b c d =
>   Quadzzz a b c d

> instance Bifunctor (Quadriceps a b) where
>   bimap fc fd (Quadzzz a b c d) =
>     Quadzzz a b (fc c) (fd d)

7.

> data EEither a b =
>     ELeft a
>   | ERight b
 
> instance Bifunctor EEither where
>   bimap fa _ (ELeft a) = ELeft $ fa a
>   bimap _ fb (ERight b) = ERight $ fb b
