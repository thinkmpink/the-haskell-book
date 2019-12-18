Chapter 16. Functor

> {-# LANGUAGE FlexibleInstances #-}

> module Functors where

> import Control.Applicative
> import Test.QuickCheck

Exercises: Be Kind
Given a type signature, determine the kinds of each type variable:

1. What's the kind of a?

f :: a -> a

The kind of a is *.

2. What are the kinds of b and T?

f :: a -> b a -> T (b a)

The kind of b is * -> * and the kind of T is * -> *.

3. What's the kind of c?

f :: c a b -> c b a

The kind of c is * -> * -> *.


Wait, how does that even typecheck?

lms :: [Maybe String]
(.) :: (b -> c) -> (a -> b) -> a -> c
fmap :: Functor f => (m -> n) -> f m -> f n
fmap :: Functor g => (x -> y) -> g x -> g y
replaceWithP :: a -> Char
fmap . fmap :: (m -> n) -> f (g m) -> f (g n)

fmap . fmap
:: ((b -> c) -> (a -> b) -> (a -> c))
   ((m -> n) -> f m -> f n)
   ((x -> y) -> g x -> g y)
:: ((a -> m -> n) -> (a -> f m -> f n))
   ((x -> y) -> g x -> g y)
:: (x -> y) -> f (g x) -> f (g y)

So how does (fmap . fmap) replaceWithP lms typecheck?


(fmap . fmap) replaceWithP lms
:: ((m -> n) -> f (g m) -> f (g n))
  (a -> Char)
  [Maybe String]
:: (f (g a) -> f (g Char))
  ([] (Maybe String))

f is bound to [], and g is bound to Maybe

:: ([] (Maybe Char))
:: [Maybe Char]



Exercises: Heavy Lifting
Add fmap, parentheses, and function composition to the expression as needed for the expression to typecheck and produce the expected result. It may not always need to go in the same place, so don’t get complacent.

1. a = (+1) $ read "[1]" :: [Int]
Expected result
     Prelude> a
     [2]

> a = fmap (+1) $ read "[1]" :: [Int]

2. b == (++ "lol") (Just ["Hi,", "Hello"])
Prelude> b
Just ["Hi,lol","Hellolol"]

> b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

3. c = (*2) (\x -> x - 2)
Prelude> c 1
-2

> c = fmap (*2) (\x -> x - 2)

4. d =
     ((return '1' ++) . show)
     (\x -> [x, 1..3])

Prelude> d 0
"1[0,1,2,3]"

> d = fmap
>   ((return '1' ++) . show)
>   (\x -> [x, 1..3])

5.

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read ("123"++) show ioi
    in (*3) changed

Prelude> e
3693

> e :: IO Integer
> e = let ioi = readIO "1" :: IO Integer
>         changed = fmap (read . ("123"++) . show) ioi
>     in fmap (*3) changed

QuickChecking Functor Instances

> functorIdentity :: (Functor f, Eq (f a))
>                 => f a -> Bool
> functorIdentity f = fmap id f == f

> functorCompose :: (Functor f, Eq (f c))
>                => (a -> b)
>                -> (b -> c)
>                -> f a
>                -> Bool
> functorCompose f g x =
>   (fmap g $ fmap f x) == fmap (g . f) x

Exercises: Instances of Functor
Implement Functor instances for the following datatypes. Use the QuickCheck properties we showed you to validate them.

1.

> newtype Identity a = Identity a
>   deriving (Eq, Show)

> instance Functor Identity where
>   fmap f (Identity a) = Identity $ f a

> instance Arbitrary a
>       => Arbitrary (Identity a) where
>   arbitrary = fmap Identity arbitrary

> testIdentityFunctor :: IO ()
> testIdentityFunctor = do
>   quickCheck
>     (functorIdentity :: Identity Int -> Bool)
>   quickCheck
>     (functorCompose (+1) (*2)
>      :: Identity Integer -> Bool)

2.

> data Pair a = Pair a a
>   deriving (Eq, Show)

> instance Functor Pair where
>   fmap f (Pair a b) = Pair (f a) (f b)

> instance Arbitrary a
>       => Arbitrary (Pair a) where
>   arbitrary = Pair <$> arbitrary <*> arbitrary

> testPairFunctor :: IO ()
> testPairFunctor = do
>   quickCheck (functorIdentity :: Pair Int -> Bool)
>   quickCheck (functorCompose (+1) (*3)
>               :: Pair Integer -> Bool)

3.

> data Two a b = Two a b
>   deriving (Eq, Show)

> instance Functor (Two a) where
>   fmap f (Two a b) = Two a $ f b

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Two a b) where
>   arbitrary = Two <$> arbitrary <*> arbitrary

> testTwoFunctor :: IO ()
> testTwoFunctor = do
>   quickCheck (functorIdentity
>               :: Two Int String -> Bool)
>   quickCheck (functorCompose (*3) (+4)
>               :: Two Int Integer -> Bool)

4.

> data Three a b c = Three a b c
>   deriving (Eq, Show)

> instance Functor (Three a b) where
>   fmap f (Three a b c) = Three a b $ f c

> instance (Arbitrary a, Arbitrary b, Arbitrary c)
>       => Arbitrary (Three a b c) where
>   arbitrary = Three
>           <$> arbitrary
>           <*> arbitrary
>           <*> arbitrary

> testThreeFunctor :: IO ()
> testThreeFunctor = do
>   quickCheck (functorIdentity
>               :: Three Int String Bool -> Bool)
>   quickCheck (functorCompose ('s':) (++"hello")
>               :: Three Int Bool String -> Bool)

5.

> data Three' a b = Three' a b b
>   deriving (Eq, Show)

> instance Functor (Three' a) where
>   fmap f (Three' a y z) = Three' a (f y) (f z)

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Three' a b) where
>   arbitrary = Three'
>           <$> arbitrary
>           <*> arbitrary
>           <*> arbitrary

> testThreePrimeFunctor :: IO ()
> testThreePrimeFunctor = do
>   quickCheck (functorIdentity
>               :: Three' String Int -> Bool)
>   quickCheck (functorCompose (+1) (*4)
>               :: Three' Bool Int -> Bool)

6.

> data Four a b c d = Four a b c d
>   deriving (Eq, Show)

> instance Functor (Four a b c) where
>   fmap f (Four a b c d) = Four a b c $ f d

> instance (Arbitrary a, Arbitrary b,
>           Arbitrary c, Arbitrary d)
>       => Arbitrary (Four a b c d) where
>   arbitrary = Four
>           <$> arbitrary
>           <*> arbitrary
>           <*> arbitrary
>           <*> arbitrary

> testFourFunctor :: IO ()
> testFourFunctor = do
>   quickCheck (functorIdentity
>     :: Four Int Bool String (Maybe Bool) -> Bool)
>   quickCheck (functorCompose (+4) (*5)
>     :: Four (Maybe Int) String Int Int -> Bool)

7.

> data Four' a b = Four' a a a b
>   deriving (Eq, Show)

> instance Functor (Four' a) where
>   fmap f (Four' a b c x) = Four' a b c $ f x

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Four' a b) where
>   arbitrary = Four'
>           <$> arbitrary
>           <*> arbitrary
>           <*> arbitrary
>           <*> arbitrary

> testFourPrimeFunctor :: IO ()
> testFourPrimeFunctor = do
>   quickCheck (functorIdentity
>     :: Four' String Integer -> Bool)
>   quickCheck (functorCompose ("hi"++) (++"bye")
>     :: Four' Int String -> Bool)

8. Can you implement one for this type? Why? Why not?
     data Trivial = Trivial

No you cannot. This type does not have kind * -> *, a requirement for any functor.


Exercise: Possibly
Write a Functor instance for a datatype identical to Maybe. We’ll use our own datatype because Maybe already has a Functor instance and we cannot make a duplicate one.

> data Possibly a =
>     LolNope
>   | Yeppers a deriving (Eq, Show)

> instance Functor Possibly where
>   fmap f (Yeppers a) = Yeppers (f a)
>   fmap _ LolNope     = LolNope

> instance Arbitrary a => Arbitrary (Possibly a) where
>   arbitrary = oneof [ return LolNope
>                     , Yeppers <$> arbitrary
>                     ]

> testPossiblyFunctor :: IO ()
> testPossiblyFunctor = do
>   quickCheck (functorIdentity
>               :: Possibly Int -> Bool)
>   quickCheck (functorCompose (+1) (*3)
>               :: Possibly Int -> Bool)

> liftedInc :: (Functor f, Num b)
>           => f b -> f b
> liftedInc = fmap (+1)

> liftedShow :: (Functor f, Show a)
>            => f a -> f String
> liftedShow = fmap show

Short Exercise
1. Write a Functor instance for a datatype identical to Either. We’ll use our own datatype because Either has a Functor instance.

> data Sum a b =
>     First a
>   | Second b
>   deriving (Eq, Show)

> instance Functor (Sum a) where
>   fmap _ (First a) = First a
>   fmap f (Second b) = Second $ f b

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Sum a b) where
>   arbitrary = oneof
>     [ First <$> arbitrary
>     , Second <$> arbitrary
>     ]

> testSumFunctor :: IO ()
> testSumFunctor = do
>   quickCheck (functorIdentity
>               :: Sum String Int -> Bool)
>   quickCheck (functorCompose (+1) (*3)
>     :: Sum String Integer -> Bool)


2. Why is a Functor instance that applies the function only to First, Either’s Left, impossible? We covered this earlier.

Because of the order of type application, the first type that must be applied to the type constructor Either is of type a, for which (Left a) is the term level witness. Since functors are defined for types of kind * -> *, we must apply a, cementing (Left a) in the untouchable "structure" of the functor, and not the value. The function therefore must be applied to the term-level witness for the b type parameter: Right b.


Chapter Exercises

Determine if a valid Functor can be written for the datatype provided.

1. data Bool =
     False | True

No valid functor exists for Bool because it is not of kind * -> *.

2.

> data BoolAndSomethingElse a =
>     False' a | True' a
>   deriving (Eq, Show)

Yes, the following functor exists for this type:

> instance Functor BoolAndSomethingElse where
>   fmap f (False' a) = False' (f a)
>   fmap f (True' a)  = True' (f a)

> instance Arbitrary a
>       => Arbitrary (BoolAndSomethingElse a) where
>   arbitrary = oneof [ False' <$> arbitrary
>                     , True' <$> arbitrary
>                     ]

> testBoolAndSomethingElseFunctor :: IO ()
> testBoolAndSomethingElseFunctor = do
>   quickCheck (functorIdentity
>     :: BoolAndSomethingElse Int -> Bool)
>   quickCheck (functorCompose (+1) (*3)
>     :: BoolAndSomethingElse Integer -> Bool)

3.

> data BoolAndMaybeSomethingElse a =
>   Falsish | Truish a
>   deriving (Eq, Show)

Yes, and the functor instance is equivalent to Maybe's.

> instance Functor BoolAndMaybeSomethingElse where
>   fmap f (Truish a) = Truish (f a)
>   fmap _ Falsish    = Falsish

> instance Arbitrary a
>       => Arbitrary (BoolAndMaybeSomethingElse a)
>   where
>     arbitrary =
>       frequency [ (1, return Falsish)
>                 , (2, Truish <$> arbitrary)
>                 ]

> testBoolAndMaybeSomethingElseFunctor :: IO ()
> testBoolAndMaybeSomethingElseFunctor = do
>   quickCheck (functorIdentity
>     :: BoolAndMaybeSomethingElse String -> Bool)
>   quickCheck (functorCompose (+12) (*13)
>     :: BoolAndMaybeSomethingElse Int -> Bool)

4. Use the kinds to guide you on this one, don’t get too hung up on the details.

> newtype Mu f = InF { outF :: f (Mu f) }

This should have a valid Functor instance because the kind of Mu is * -> *. I'm not sure what the instance is, though.

5. Again, follow the kinds and ignore the unfamiliar parts

import GHC.Arr

data D =
  D (Array Word Word) Int Int

This does not have a Functor instance because D has kind *.


Rearrange the arguments to the type constructor of the datatype so the Functor instance works.

1.

data Sum a b =
    First a
  | Second b

> data Sum' b a =
>     First' a
>   | Second' b
>   deriving (Eq, Show)

> instance Functor (Sum' e) where
>   fmap f (First' a)  = First' (f a)
>   fmap f (Second' b) = Second' b

2.

data Company a b c =
    DeepBlue a c
  | Something b

> data Company a c b =
>     DeepBlue a c
>   | Something b
>   deriving (Eq, Show)

> instance Functor (Company e e') where
>   fmap f (Something b) = Something (f b)
>   fmap _ (DeepBlue a c) = DeepBlue a c

3.
data More a b =
    L a b a
  | R b a b
  deriving (Eq, Show)

> data More b a =
>     L a b a
>   | R b a b
>   deriving (Eq, Show)
>
> instance Functor (More x) where
>   fmap f (L a b a') = L (f a) b (f a')
>   fmap f (R b a b') = R b (f a) b'

Write Functor instances for the following datatypes.
1.

> data Quant a b =
>     Finance
>   | Desk a
>   | Bloor b
>   deriving (Eq, Show)

> instance Functor (Quant a) where
>   fmap f (Bloor b) = Bloor $ f b
>   fmap _ (Desk a)  = Desk a
>   fmap _ Finance   = Finance

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Quant a b) where
>   arbitrary = oneof [ return Finance
>                     , Desk <$> arbitrary
>                     , Bloor <$> arbitrary
>                     ]

> testQuantFunctor :: IO ()
> testQuantFunctor = do
>   quickCheck (functorIdentity
>     :: Quant Int String -> Bool)
>   quickCheck (functorCompose (+4) (*13)
>     :: Quant String Integer -> Bool)

2. No, it's not interesting by itself.

> data K a b =
>   K a
>   deriving (Eq, Show)

> instance Functor (K a) where
>   fmap _ (K a) = (K a)

> instance Arbitrary a
>       => Arbitrary (K a b) where
>   arbitrary = K <$> arbitrary

> testKFunctor :: IO ()
> testKFunctor = do
>   quickCheck (functorIdentity
>     :: K [Bool] Int -> Bool)
>   quickCheck (functorCompose ("h" ++) (take 3)
>     :: K Int String -> Bool)

3.
{-# LANGUAGE FlexibleInstances #-}

> newtype Flip f a b =
>   Flip (f b a)
>   deriving (Eq, Show)

> newtype K' a b = K' a
>   deriving (Eq, Show)

> -- should remind you of an
> -- instance you've written before
> instance Functor (Flip K' a) where
>   fmap f (Flip (K' a)) = Flip $ K' $ f a

4.

> data EvilGoateeConst a b =
>   GoatyConst b
>   deriving (Eq, Show)

> instance Functor (EvilGoateeConst a) where
>   fmap f (GoatyConst b) = GoatyConst $ f b

> instance Arbitrary b
>       => Arbitrary (EvilGoateeConst a b) where
>   arbitrary = GoatyConst <$> arbitrary

> testEvilGoateeConstFunctor :: IO ()
> testEvilGoateeConstFunctor = do
>   quickCheck (functorIdentity
>     :: EvilGoateeConst [Bool] Int -> Bool)
>   quickCheck (functorCompose ("h" ++) (take 3)
>     :: EvilGoateeConst Int String -> Bool)


5. Do you need something extra to make the instance work?

> data LiftItOut f a =
>   LiftItOut (f a)
>   deriving (Eq, Show)

> instance Functor f
>       => Functor (LiftItOut f) where
>   fmap g (LiftItOut f) =
>     LiftItOut $ g <$> f

> instance Arbitrary (f a)
>       => Arbitrary (LiftItOut f a) where
>   arbitrary = LiftItOut <$> arbitrary

> testLiftItOutFunctor :: IO ()
> testLiftItOutFunctor = do
>   quickCheck (functorIdentity
>     :: LiftItOut Maybe Int -> Bool)
>   quickCheck (functorCompose (+3) (*4)
>     :: LiftItOut Maybe Int -> Bool)

6.

> data Parappa f g a =
>   DaWrappa (f a) (g a)
>   deriving (Eq, Show)

> instance (Functor f, Functor g)
>       => Functor (Parappa f g) where
>   fmap f (DaWrappa a b) =
>     DaWrappa (f <$> a) (f <$> b)

> instance (Arbitrary (f a), Arbitrary (g a))
>       => Arbitrary (Parappa f g a) where
>   arbitrary = DaWrappa <$> arbitrary <*> arbitrary

> testParappaFunctor :: IO ()
> testParappaFunctor = do
>   quickCheck (functorIdentity
>     :: Parappa Maybe (Either Int) String -> Bool)
>   quickCheck (functorCompose (*3) (+4)
>     :: Parappa (Either String) [] Integer -> Bool)

7. Don’t ask for more type class instances than you need. You can let GHC tell you what to do.

> data IgnoreOne f g a b =
>   IgnoringSomething (f a) (g b)
>   deriving (Eq, Show)

> instance Functor g
>       => Functor (IgnoreOne f g a) where
>   fmap h (IgnoringSomething f g) =
>     IgnoringSomething f $ h <$> g

> instance (Arbitrary (f a), Arbitrary (g b))
>       => Arbitrary (IgnoreOne f g a b) where
>   arbitrary = IgnoringSomething
>           <$> arbitrary
>           <*> arbitrary

> testIgnoreOneFunctor :: IO ()
> testIgnoreOneFunctor = do
>   quickCheck (functorIdentity
>     :: IgnoreOne Maybe [] Bool (Maybe Char)
>     -> Bool)
>   quickCheck (functorCompose (+5) (*2)
>     :: IgnoreOne [] (Either String) Int Int
>     -> Bool)

8.

> data Notorious g o a t =
>   Notorious (g o) (g a) (g t)
>   deriving (Eq, Show)

> instance Functor x
>       => Functor (Notorious x y z) where
>   fmap f (Notorious b i g) =
>     Notorious b i $ fmap f g

> instance (Arbitrary (g o), Arbitrary (g a),
>           Arbitrary (g t))
>       => Arbitrary (Notorious g o a t) where
>   arbitrary = Notorious
>           <$> arbitrary
>           <*> arbitrary
>           <*> arbitrary

> testNotoriousFunctor :: IO ()
> testNotoriousFunctor = do
>   quickCheck (functorIdentity
>     :: Notorious [] Int String Bool -> Bool)
>   quickCheck (functorCompose (*2) (+7)
>     :: Notorious (Either String) Int Bool Int
>     -> Bool)

9. You'll need to use recursion.

> data List a =
>     Nil
>   | Cons a (List a)
>   deriving (Eq, Show)

> instance Functor List where
>   fmap _ Nil         = Nil
>   fmap f (Cons a as) = Cons (f a) (f <$> as)

> instance Arbitrary a
>       => Arbitrary (List a) where
>   arbitrary =
>     oneof [ return Nil
>           , Cons <$> arbitrary <*> arbitrary
>           ]

> testListFunctor :: IO ()
> testListFunctor = do
>   quickCheck (functorIdentity
>     :: List Int -> Bool)
>   quickCheck (functorCompose (+3) (*4)
>     :: List Integer -> Bool)

10. A tree of goats forms a Goat-Lord, fearsome poly-creature.

> data GoatLord a =
>     NoGoat
>   | OneGoat a
>   | MoreGoats (GoatLord a)
>               (GoatLord a)
>               (GoatLord a)
>   deriving (Eq, Show)

> instance Functor GoatLord where
>   fmap _ NoGoat      = NoGoat
>   fmap f (OneGoat a) = OneGoat $ f a
>   fmap f (MoreGoats a b c) =
>     MoreGoats (f <$> a)
>               (f <$> b)
>               (f <$> c)

> instance Arbitrary a
>       => Arbitrary (GoatLord a) where
>   arbitrary =
>     oneof [ return NoGoat
>           , OneGoat <$> arbitrary
>           , MoreGoats <$> arbitrary
>                       <*> arbitrary
>                       <*> arbitrary
>           ]

> testGoatLordFunctor :: IO ()
> testGoatLordFunctor = do
>   quickCheck (functorIdentity
>     :: GoatLord Int -> Bool)
>   quickCheck (functorCompose (*3) (+2)
>     :: GoatLord Integer -> Bool)

11. You’ll use an extra functor for this one, although your solution might do it monomorphically without using fmap. Keep in mind that you will probably not be able to validate this one in the usual manner. Do your best to make it work.

> data TalkToMe a =
>     Halt
>   | Print  String a
>   | Read  (String -> a)

> instance Show a
>       => Show (TalkToMe a) where
>   show Halt = "Halt"
>   show (Print s a) = "Print " ++ show s ++ show a
>   show (Read f) = "Read " ++
>     (show $ functionShow f)

> instance Functor TalkToMe where
>   fmap _ Halt        = Halt
>   fmap f (Print s a) = Print s $ f a
>   fmap f (Read g)    = Read $ f <$> g

> unRead :: TalkToMe a -> String -> Maybe a
> unRead (Read s)    t = Just (s t)
> unRead (Print _ a) _ = Just a
> unRead _           _ = Nothing

> instance Arbitrary a
>       => Arbitrary (TalkToMe a) where
>   arbitrary = oneof
>     [ return Halt
>     , Print <$> arbitrary <*> arbitrary
>     , Read <$> arbitrary
>     ]

> functorFunIdentity :: (Functor f, Eq a)
>                    => (f b -> a)
>                    -> f b
>                    -> Bool
> functorFunIdentity g f =
>   g f == g (fmap id f)

> functorFunCompose :: (Functor f, Eq d)
>                   => (f c -> d)
>                   -> (a -> b)
>                   -> (b -> c)
>                   -> f a
>                   -> Bool
> functorFunCompose toD toB toC f =
>      toD (fmap toC . fmap toB $ f)
>   == toD (fmap (toC . toB) f)

> testTalkToMeFunctor :: IO ()
> testTalkToMeFunctor = do
>   quickCheck $ forAll
>     (arbitrary :: Gen String) $
>     \s -> (functorFunIdentity (($ s) . unRead)
>          :: TalkToMe Int -> Bool)
>   quickCheck $ forAll
>     (arbitrary :: Gen String) $
>     \s -> (functorFunCompose
>             (($ s) . unRead)
>             (+4)
>             (*3)
>             :: TalkToMe Int -> Bool)
