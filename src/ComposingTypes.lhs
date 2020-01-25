Chapter 25. Composing Types

> {-# LANGUAGE InstanceSigs #-}
>
> module ComposingTypes where
>
> import Test.Hspec

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
>   -- Compose [Just (+2), Just (+1)] <*>
>   --   Compose [Just 4, Nothing]
>   -- = Compose [Just 6, Nothing, Just 5, Nothing]
>     Compose $ fmap (<*>) f <*> a 
