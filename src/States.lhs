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

Fizzbuzz Differently
It’s an exercise! Rather than changing the underlying data structure, fix our reversing fizzbuzz by changing the code in the following way:

fizzbuzzFromTo :: Integer
               -> Integer
               -> [String]
fizzbuzzFromTo = undefined

see fizzBuzz.hs

Chapter Exercises

23.8 Chapter exercises
Write the following functions. You’ll want to use your own State type for which you’ve defined the Functor, Applicative, and Monad.

I'm going to reuse the Moi type.

1. Construct a State where the state is also the value you return.

> get :: Moi s s
> get = Moi $ \s -> (s, s)

2. Construct a State where the resulting state is the argument provided and the value is defaulted to unit.

> put :: s -> Moi s ()
> put s = Moi $ \_ -> ((), s)

3. Run the State with 𝑠 and get the state that results.

> exec :: Moi s a -> s -> s
> exec (Moi sa) = snd . sa

4. Run the State with s and get the value that results.

> eval :: Moi s a -> s -> a
> eval (Moi sa) = fst . sa

5. Write a function which applies a function to create a new State.

> modify :: (s -> s) -> Moi s ()
> modify f = Moi $ \s -> ((), f s)
