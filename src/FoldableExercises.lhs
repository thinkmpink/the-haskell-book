you can make any comments

> {-# LANGUAGE InstanceSigs #-}
>
> module FoldableExercises where
>
>
> data Constant t r =
>   Constant r
>
> instance Foldable (Constant t) where
>   foldr :: (a -> b -> b) -> b -> Constant t a -> b
>   foldr f b (Constant r) = f r b
>
> data Two a b = Two a b
>
> instance Foldable (Two a) where
>   foldMap f (Two a b) = f b
>
> data Three a b c =
>   Three a b c
>
> instance Foldable (Three a b) where
>   foldMap f (Three a b c) = f c
>
> data Thre a b = Thre a b b
>
> instance Foldable (Thre a) where
>   foldr f b (Thre a b1 b2) = f b1 b
>
> data Four' a b =
>   Four' a b b b
>
> instance Foldable (Four' a) where
>   foldr f b (Four' a b1 b2 b3) = f b3 b
>
> filterF :: (Applicative f, Foldable t, Monoid (f a))
>         => (a -> Bool) -> t a -> f a

> filterF f t = foldMap (\a -> if f a then pure a else mempty) t

filterF f t = foldMap _ t






hello
