
> module TypeClasses where

Exercises: Eq Instances
Write the Eq instance for the datatype provided.

1. It’s not a typo, we’re just being cute with the name.

> data TisAnInteger =
>   TisAn Integer
>

> instance Eq TisAnInteger where
>   (==) (TisAn a) (TisAn b) = a == b
>

2.

> data TwoIntegers = Two' Integer Integer

> instance Eq TwoIntegers where
>   Two' a b == Two' c d = a == c && b == d

3.

> data StringOrInt =
>     TisAnInt   Int
>   | TisAString String

> instance Eq StringOrInt where
>   (==) (TisAnInt a)   (TisAnInt b) = a == b
>   (==) (TisAString a) (TisAString b) = a == b
>   (==) _              _              = False

4.

> data Pair a =
>   Pair a a
>
> instance Eq a => Eq (Pair a) where
>   (==) (Pair a b) (Pair c d) = a == c && b == d

5.

> data Tuple a b =
>   Tuple a b
>
> instance (Eq a, Eq b) => Eq (Tuple a b) where
>   (==) (Tuple a b) (Tuple c d) = a == c && b == d
>

6.

> data Which a =
>     ThisOne a
>   | ThatOne a
>
> instance Eq a => Eq (Which a) where
>   (==) (ThisOne a) (ThisOne b) = a == b
>   (==) (ThatOne a) (ThatOne b) = a == b
>   (==) _           _           = False
>

7.

> data EitherOr a b =
>     Hello a
>   | Goodbye b

> instance (Eq a, Eq b) => Eq (EitherOr a b) where
>   (==) (Hello a)   (Hello b)   = a == b
>   (==) (Goodbye a) (Goodbye b) = a == b
>   (==) _           _           = False
>

Exercises: Tuple Experiment Look at the types given for quotRem and divMod. What do you think those functions do? Test your hypothe- ses by playing with them in the REPL. We’ve given you a sample to start with below:
Prelude> ones x = snd (divMod x 10)

mp> :t div
div :: Integral a => a -> a -> a
mp> :t mod
mod :: Integral a => a -> a -> a
mp> :t divMod
divMod :: Integral a => a -> a -> (a, a)
mp> div 3 5
0
mp> div 5 3
1
mp> mod 5 3
2
mp> divMod 5 3
(1,2)
mp>
mp> :t quot
quot :: Integral a => a -> a -> a
mp> :t rem
rem :: Integral a => a -> a -> a
mp> :t quotRem
quotRem :: Integral a => a -> a -> (a, a)
mp> divMod 5 (-3)
(-2,-1)
mp> quotRem 5 (-3)
(-1,2)
mp> divMod 5 3
(1,2)
mp> quotRem 5 3
(1,2)
mp>
