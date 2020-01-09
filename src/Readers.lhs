Chapter 22. Reader

> {-# LANGUAGE InstanceSigs #-}

> module Readers where
>
> import Control.Applicative

See first code snippet in bip.hs.
See second in reader1.hs

> newtype Reader r a =
>   Reader { runReader :: r -> a }

> instance Functor (Reader r) where
>   fmap f (Reader ra) =
>     Reader $ f . ra

Exercise: Ask
Implement the following function. If you get stuck, remember it’s less complicated than it looks. Write down what you know. What do you know about the type 𝑎? What does the type simplify to? How many inhabitants does that type have? You’ve seen the type before.


> ask :: Reader a a
> ask = Reader id

We know nothing about the type a, and Reader a a is simply a newtype wrapper around a function f :: a -> a. The only such function, which must be fully polymorphic, is id.

Exercise: Reading Comprehension
1. Write liftA2 yourself. Think about it in terms of abstracting out the difference between getDogR and getDogR' if that helps.

> myLiftA2 :: Applicative f =>
>            (a -> b -> c)
>         -> f a -> f b -> f c
> myLiftA2 f a b = f <$> a <*> b

2. Write the following function. Again, it is simpler than it looks.

> asks :: (r -> a) -> Reader r a
> asks f = Reader f

3. Implement the Applicative for Reader.
To write the Applicative instance for Reader, we’ll use an extension called InstanceSigs. It’s an extension we need in order to assert a type for the type class methods. You ordinarily cannot assert type signatures in instances. The compiler already knows the type of the functions, so it’s not usually necessary to assert the types in instances anyway. We did this for the sake of clarity, to make the Reader type explicit in our signatures.

> instance Applicative (Reader r) where
>   pure :: a -> Reader r a
>   pure a = Reader $ const a
>
>   (<*>) :: Reader r (a -> b)
>         -> Reader r a
>         -> Reader r b
>   Reader rab <*> Reader ra =
>     Reader $ \r -> rab r (ra r)

Exercise: Reader Monad
1. Implement the Reader Monad.

> instance Monad (Reader r) where
>   (>>=) :: Reader r a
>         -> (a -> Reader r b)
>         -> Reader r b
>   (Reader ra) >>= aRb =
>     Reader $ \r -> runReader (aRb $ ra r) r

2. Rewrite the monadic getDogRM to use your Reader datatype.

> newtype HumanName =
>   HumanName String
>   deriving (Eq, Show)
>
> newtype DogName =
>   DogName String
>   deriving (Eq, Show)
>
> newtype Address =
>   Address String
>   deriving (Eq, Show)
>
> data Person =
>   Person {
>     humanName :: HumanName
>   , dogName :: DogName
>   , address :: Address
>   } deriving (Eq, Show)
>
> data Dog =
>   Dog {
>     dogsName :: DogName
>   , dogsAddress :: Address
>   } deriving (Eq, Show)
>
> -- without Reader
> getDog :: Person -> Dog
> getDog p =
>   Dog (dogName p) (address p)
>
> -- with Reader
> getDogR :: Person -> Dog
> getDogR =
>   Dog <$> dogName <*> address
>
> -- with Reader, alternate
> getDogR' :: Person -> Dog
> getDogR' =
>   liftA2 Dog dogName address
>
> -- with Reader Monad
> getDogRM :: Person -> Dog
> getDogRM = do
>   name <- dogName
>   addy <- address
>   return $ Dog name addy
>
> -- with my datatype
> getDogRM' :: Reader Person Dog
> getDogRM' = do
>   name <- Reader dogName
>   addy <- Reader address
>   return $ Dog name addy

22.11 Chapter Exercises
A warm-up stretch
These exercises are designed to be a warm-up and get you using some of the stuff we’ve learned in the last few chapters. While these exercises comprise code fragments from real code, they are simplified in order to be discrete exercises. That will allow us to highlight and practice some of the type manipulation from Traversable and Reader, both of which are tricky.

see ReaderPractice.hs
