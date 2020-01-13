Chapter 23. State

> {-# LANGUAGE InstanceSigs #-}
>
> module States where
>
> import Test.QuickCheck
> import Test.QuickCheck.Checkers
> import Test.QuickCheck.Classes


Write State for yourself

> newtype Moi s a =
>   Moi { runMoi :: s -> (a, s) }

> instance (Show s, Read s, Show a)
>       => Show (Moi s a) where
>   show (Moi g) = "Moi " <> show (functionShow g)

State Functor
Implement the Functor instance for State.

> instance Functor (Moi s) where
>   fmap :: (a -> b) -> Moi s a -> Moi s b
>   fmap f (Moi g) = Moi $ \s ->
>     let (a, s') = g s
>     in  (f a, s')

State Applicative
Write the Applicative instance for State.

> instance Applicative (Moi s) where
>   pure :: a -> Moi s a
>   pure a = Moi $ \s -> (a, s)
>
>   (<*>) :: Moi s (a -> b)
>         -> Moi s a
>         -> Moi s b
>   Moi f <*> Moi g = Moi $ \s ->
>     let (fab, s1) = f s
>         (a,   s2) = g s1
>     in  (fab a, s2)

State Monad
Write the Monad instance for State

> instance Monad (Moi s) where
>   (>>=) :: Moi s a
>         -> (a -> Moi s b)
>         -> Moi s b
>   Moi f >>= g = Moi $ \s ->
>     let (a1, s1) = f s
>     in  runMoi (g a1) s1

> instance (CoArbitrary s, Arbitrary a,
>           Arbitrary s)
>       => Arbitrary (Moi s a) where
>   arbitrary = Moi <$> arbitrary
>
> instance (EqProp s, EqProp a, Show s,
>           Arbitrary s, Eq a, Eq s)
>       => EqProp (Moi s a) where
>   Moi a =-= Moi b =
>     forAll arbitrary (\s -> a s == b s)

> type TMoi = Moi [Int]

> testMoiMonad :: IO ()
> testMoiMonad = do
>   let trigger :: TMoi (Int, Int, [Int])
>       trigger = undefined
>   quickBatch (functor trigger)
>   quickBatch (applicative trigger)
>   quickBatch (monad trigger)
