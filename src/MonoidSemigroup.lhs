Chapter 15. Monoid, Semigroup

> module MonoidSemigroup where
>
> import Control.Applicative
> import Control.Monad
> import Data.Monoid
> import Test.QuickCheck
> import Test.Hspec

Exercise: Optional Monoid
Write the Monoid instance for our Maybe type renamed to Optional.

> data Optional a =
>     Nada
>   | Only a
>   deriving (Eq, Show)
>
> instance Monoid a
>       => Monoid (Optional a) where
>
>   mempty  = Nada
>
> instance Semigroup a
>       => Semigroup (Optional a) where
>
>   (<>) Nada     (Only a) = Only a
>   (<>) (Only a) Nada     = Only a
>   (<>) (Only a) (Only b) = Only (a <> b)
>   (<>) _        _        = Nada

 instance Arbitrary a
       => Arbitrary (Optional a) where
   arbitrary = genOptional

 genOptional :: Arbitrary a => Gen (Optional a)
 genOptional = do
   a <- arbitrary
   oneof [ return Nada
         , return $ Only a
         ]

mp> Only (Sum 1) <> Only (Sum 1)
Only (Sum {getSum = 2})
mp> Only (Product 4) `mappend` Only (Product 2)
Only (Product {getProduct = 8})
mp> Only (Sum 1) `mappend` Nada
Only (Sum {getSum = 1})
mp> Only [1] `mappend` Nada
Only [1]

Exercise. Refactor Madlib

> type Adjective = String
> type Adverb = String
> type Noun = String
> type Exclamation = String

> madlibbin' :: Exclamation
>            -> Adverb
>            -> Noun
>            -> Adjective
>            -> String
> madlibbin' e adv noun adj =
>   e   <> "! he said " <>
>   adv <> " as he jumped into his car " <>
>   noun <> " and drove off with his " <>
>   adj <> " wife."

Now you’re going to refactor this code a bit! Rewrite it using mconcat.

> madlibbinBetter' :: Exclamation
>                  -> Adverb
>                  -> Noun
>                  -> Adjective
>                  -> String
> madlibbinBetter' e adv noun adj = mconcat
>   [e, "! he said ", adv,
>    " as he jumped into his car ",
>    noun, " and drove off with his ",
>    adj, " wife."]

Using QuickCheck to help show laws hold:

> asc :: Eq a
>     => (a -> a -> a)
>     -> a -> a -> a
>     -> Bool
> asc (<>) a b c =
>  a <> (b <> c) == (a <> b) <> c

> monoidAssoc :: (Eq m, Monoid m)
>             => m -> m -> m -> Bool
> monoidAssoc = asc (<>)

> monoidLeftIdentity :: (Eq m, Monoid m)
>                    => m -> Bool
> monoidLeftIdentity a = (mempty <> a) == a

> monoidRightIdentity :: (Eq m, Monoid m)
>                     => m -> Bool
> monoidRightIdentity a = (a <> mempty) == a

Using QuickCheck to identify an invalid Monoid:

> data Bull =
>     Fools
>   | Twoo
>   deriving (Eq, Show)

> instance Arbitrary Bull where
>   arbitrary =
>     frequency [ (1, return Fools)
>               , (1, return Twoo)]
>
> instance Semigroup Bull where
>   (<>) _ _ = Fools
>
> instance Monoid Bull where
>   mempty = Fools
>
> type BullMappend =
>   Bull -> Bull -> Bull -> Bool
>
> main :: IO ()
> main = do
>   let ma = monoidAssoc
>       mli = monoidLeftIdentity
>       mri = monoidRightIdentity
>   quickCheck (ma :: BullMappend)
>   quickCheck (mli :: Bull -> Bool)
>   quickCheck (mri :: Bull -> Bool)

Exercise: Maybe Another Monoid
Write a Monoid instance for a Maybe type which doesn’t require a Monoid for the contents. Reuse the Monoid law QuickCheck properties and use them to validate the instance.
Don’t forget to write an Arbitrary instance for First'. We won’t always stub that out explicitly for you. We suggest learning how to use the frequency function from QuickCheck for First'’s instance.

> newtype First' a =
>   First' { getFirst' :: Optional a }
>   deriving (Eq, Show)
>
> instance Semigroup (First' a) where
>   (<>) (First' (Only a)) _ = First' $ Only a
>   (<>) _                 b = b
>
> instance Monoid (First' a) where
>   mempty = First' Nada
>
> instance Arbitrary a
>       => Arbitrary (First' a) where
>   arbitrary = genFirst

> genFirst :: Arbitrary a => Gen (First' a)
> genFirst = do
>   a <- arbitrary
>   frequency [ (3, return $ First' $ Only a)
>             , (1, return $ First' $ Nada)
>             ]

> firstMappend :: First' a
>              -> First' a
>              -> First' a
> firstMappend = mappend
>
> type FirstMappend =
>      First' String
>   -> First' String
>   -> First' String
>   -> Bool
>
> type FstId =
>   First' String -> Bool
>
> main' :: IO ()
> main' = do
>   quickCheck (monoidAssoc :: FirstMappend)
>   quickCheck (monoidLeftIdentity :: FstId)
>   quickCheck (monoidRightIdentity :: FstId)


Chapter exercises

Semigroup exercises
Given a datatype, implement the Semigroup instance. Add Semigroup constraints to type variables where needed.

Note We’re not always going to derive every instance you may want or need in the datatypes we provide for exercises. We expect you to know what you need and to take care of it yourself by this point.


1. Validate all of your instances with QuickCheck. Since Semigroup’s only law is associativity, that’s the only property you need to reuse.

> data Trivial = Trivial
>   deriving (Eq, Show)

> instance Semigroup Trivial where
>   _ <> _ = Trivial

> instance Arbitrary Trivial where
>   arbitrary = return Trivial

> semigroupAssoc :: (Eq m, Semigroup m)
>                => m -> m -> m -> Bool
> semigroupAssoc a b c =
>   (a <> (b <> c)) == ((a <> b) <> c)

> type TrivAssoc =
>   Trivial -> Trivial -> Trivial -> Bool
>
> testTrivAssoc :: IO ()
> testTrivAssoc =
>   quickCheck (semigroupAssoc :: TrivAssoc)

2.

> newtype Identity a = Identity a
>   deriving (Eq, Show)

> instance Semigroup a
>       => Semigroup (Identity a) where
>   Identity a <> Identity b = Identity $ a <> b

> instance Arbitrary a
>       => Arbitrary (Identity a) where
>   arbitrary = fmap Identity arbitrary

> type IdentityAssoc a =
>   Identity a -> Identity a -> Identity a -> Bool

> testIdentityAssoc :: IO ()
> testIdentityAssoc =
>   quickCheck (semigroupAssoc :: IdentityAssoc String)

3.

> data Two a b = Two a b
>   deriving (Eq, Show)

> instance (Semigroup a, Semigroup b)
>       => Semigroup (Two a b) where
>   Two a b <> Two c d = Two (a <> c) (b <> d)

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Two a b) where
>   arbitrary = do
>     a <- arbitrary
>     b <- arbitrary
>     return $ Two a b

> type TwoAssoc a b =
>   Two a b -> Two a b -> Two a b -> Bool

> testTwoAssoc :: IO ()
> testTwoAssoc =
>   quickCheck
>     (semigroupAssoc :: TwoAssoc String String)

4.

> data Three a b c = Three a b c
>   deriving (Eq, Show)

> instance (Semigroup a, Semigroup b, Semigroup c)
>       => Semigroup (Three a b c) where
>   Three a b c <> Three d e f =
>     Three (a <> d) (b <> e) (c <> f)

> instance (Arbitrary a, Arbitrary b, Arbitrary c)
>       => Arbitrary (Three a b c) where
>   arbitrary = Three <$> arbitrary
>                     <*> arbitrary
>                     <*> arbitrary

> type ThreeAssoc a b c =
>      Three a b c
>   -> Three a b c
>   -> Three a b c
>   -> Bool

> testThreeAssoc :: IO ()
> testThreeAssoc =
>   quickCheck (semigroupAssoc
>     :: ThreeAssoc String String String)

5.

> data Four a b c d = Four a b c d
>   deriving (Eq, Show)

> instance (Semigroup a, Semigroup b,
>           Semigroup c, Semigroup d)
>       => Semigroup (Four a b c d) where
>   Four a b c d <> Four e f g h =
>     Four (a <> e) (b <> f) (c <> g) (d <> h)

> instance (Arbitrary a, Arbitrary b,
>           Arbitrary c, Arbitrary d)
>       => Arbitrary (Four a b c d) where
>   arbitrary =
>     Four <$> arbitrary
>          <*> arbitrary
>          <*> arbitrary
>          <*> arbitrary

> type FourAssoc a b c d =
>      Four a b c d
>   -> Four a b c d
>   -> Four a b c d
>   -> Bool

> testFourAssoc :: IO ()
> testFourAssoc =
>   quickCheck (semigroupAssoc ::
>     FourAssoc String
>               (First Int)
>               (Last Int)
>               (Sum Integer))

6.

> newtype BoolConj =
>   BoolConj Bool
>   deriving (Eq, Show)

> instance Semigroup BoolConj where
>   BoolConj a <> BoolConj b = BoolConj $ a && b

> instance Arbitrary BoolConj where
>   arbitrary = BoolConj <$> arbitrary

> type BoolConjAssoc =
>   BoolConj -> BoolConj -> BoolConj -> Bool

> testBoolConjAssoc :: IO ()
> testBoolConjAssoc =
>   quickCheck (semigroupAssoc :: BoolConjAssoc)

7.

> newtype BoolDisj = BoolDisj Bool
>   deriving (Eq, Show)

> instance Semigroup BoolDisj where
>   BoolDisj a <> BoolDisj b = BoolDisj $ a || b

> instance Arbitrary BoolDisj where
>   arbitrary = BoolDisj <$> arbitrary

> type BoolDisjAssoc =
>   BoolDisj -> BoolDisj -> BoolDisj -> Bool

> testBoolDisjAssoc :: IO ()
> testBoolDisjAssoc = quickCheck
>   (semigroupAssoc :: BoolDisjAssoc)

8.

> data Or a b =
>     Fst a
>   | Snd b
>   deriving (Eq, Show)

> instance Semigroup (Or a b) where
>   Fst _ <> Snd b = Snd b
>   Fst _ <> Fst a = Fst a
>   Snd b <> Fst _ = Snd b
>   Snd a <> Snd _ = Snd a

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Or a b) where
>   arbitrary = oneof [ Fst <$> arbitrary
>                     , Snd <$> arbitrary
>                     ]

> type OrAssoc a b =
>   Or a b -> Or a b -> Or a b -> Bool

> testOrAssoc :: IO ()
> testOrAssoc = quickCheck
>   (semigroupAssoc :: OrAssoc Int Bool)

9.

> newtype Combine a b =
>   Combine { unCombine :: (a -> b) }


> instance (Show a, Read a, Show b)
>       => Show (Combine a b) where
>   show = ("Combine " ++)
>        . show . functionShow . unCombine

> instance Semigroup b
>       => Semigroup (Combine a b) where
>   Combine f <> Combine g = Combine $ \a -> f a <> g a

> instance (CoArbitrary a, Arbitrary b)
>       => Arbitrary (Combine a b) where
>   arbitrary = Combine <$> arbitrary

> testCombine :: IO ()
> testCombine = quickCheck $
>   forAll (arbitrary :: Gen (Combine Int String)) $ \a ->
>     forAll (arbitrary :: Gen (Combine Int String)) $ \b ->
>       forAll (arbitrary :: Gen (Combine Int String)) $ \c ->
>         forAll (arbitrary :: Gen Int) $ \i ->
>              (unCombine ((a <> b) <> c) i)
>           == (unCombine (a <> (b <> c)) i)

10.

> newtype Comp a =
>   Comp { unComp :: (a -> a) }

> instance (Show a, Read a)
>       => Show (Comp a) where
>   show = ("Comp " ++) . show . functionShow . unComp

> instance Semigroup (Comp a) where
>   Comp a <> Comp b = Comp $ a . b

> instance (CoArbitrary a, Arbitrary a)
>       => Arbitrary (Comp a) where
>   arbitrary = Comp <$> arbitrary

> testCompAssoc :: IO ()
> testCompAssoc = quickCheck $ forAll
>   (arbitrary :: Gen (Comp Int, Comp Int, Comp Int)) $
>   \(a, b, c) ->
>     forAll (arbitrary :: Gen Int) $ \i ->
>              (unComp ((a <> b) <> c) i)
>           == (unComp (a <> (b <> c)) i)

11.

> data Validation a b =
>   Failure' a | Success' b
>   deriving (Eq, Show)

> instance Semigroup a =>
>   Semigroup (Validation a b) where
>     Success' b <> Failure' _ = Success' b
>     Failure' a <> Failure' b = Failure' $ a <> b
>     Success' a <> Success' _ = Success' a
>     Failure' _ <> Success' b = Success' b

> instance (Arbitrary a, Arbitrary b)
>       => Arbitrary (Validation a b) where
>   arbitrary =
>     oneof [ Failure' <$> arbitrary
>           , Success' <$> arbitrary
>           ]

> type ValidationAssoc a b =
>      Validation a b
>   -> Validation a b
>   -> Validation a b
>   -> Bool

> testValidationAssoc :: IO ()
> testValidationAssoc = do
>   let failure :: String -> Validation String Int
>       failure = Failure'
>       success :: Int -> Validation String Int
>       success = Success'
>   hspec $ do
>     describe "Semigroup instance for Validation" $ do
>       it "picks left success" $ do
>         success 1 <> failure "blah" `shouldBe`
>           Success' 1
>       it "combines failures via their semigroup op" $ do
>         failure "woot" <> failure "blah" `shouldBe`
>           Failure' "wootblah"
>       it "picks the first success of many" $ do
>         success 1 <> success 2 `shouldBe`
>           Success' 1
>       it "picks a success after a failure" $ do
>         failure "woot" <> success 2 `shouldBe`
>           Success' 2
>       it "is associative" $ do
>         property
>           (semigroupAssoc
>            :: ValidationAssoc String Integer)



Monoid exercises
Given a datatype, implement the Monoid instance. Add Monoid con- straints to type variables where needed. For the datatypes you’ve already implemented Semigroup instances for, you need to figure out what the identity value is.

1. Again, validate all of your instances with QuickCheck. Example scaffold is provided for the Trivial type.

> instance Monoid Trivial where
>   mempty = Trivial

> testTrivialMonoid :: IO ()
> testTrivialMonoid = do
>   let sa = semigroupAssoc
>       mli = monoidLeftIdentity
>       mlr = monoidRightIdentity
>   quickCheck (sa :: TrivAssoc)
>   quickCheck (mli :: Trivial -> Bool)
>   quickCheck (mlr :: Trivial -> Bool)

2.

> instance Monoid a
>       => Monoid (Identity a) where
>   mempty = Identity mempty

> testIdentityMonoid :: IO ()
> testIdentityMonoid = hspec $ do
>   describe "Identity monoid" $ do
>     it "is associative" $ do
>       property (semigroupAssoc :: IdentityAssoc String)
>     it "has left id" $ do
>       property
>         (monoidLeftIdentity :: Identity String -> Bool)
>     it "has right id" $ do
>       property
>         (monoidRightIdentity :: Identity String -> Bool)

3.

> instance (Monoid a, Monoid b)
>       => Monoid (Two a b) where
>   mempty = Two mempty mempty

> testTwoMonoid :: IO ()
> testTwoMonoid = hspec $ do
>   describe "Two monoid" $ do
>     it "is associative" $ do
>       property (semigroupAssoc
>             :: TwoAssoc String (First Int))
>     it "has left id" $ do
>       property
>         (monoidLeftIdentity
>          :: Two String (Last Int) -> Bool)
>     it "has right id" $ do
>       property
>         (monoidRightIdentity :: Two String String -> Bool)

4.

> instance Monoid BoolConj where
>   mempty = BoolConj True

> testBoolConjMonoid :: IO ()
> testBoolConjMonoid = hspec $ do
>   describe "BoolConj monoid" $ do
>     it "is associative" $ do
>       property (semigroupAssoc:: BoolConjAssoc)
>     it "has left id" $ do
>       property
>         (monoidLeftIdentity :: BoolConj -> Bool)
>     it "has right id" $ do
>       property
>         (monoidRightIdentity :: BoolConj -> Bool)

5.

> instance Monoid BoolDisj where
>   mempty = BoolDisj False

> testBoolDisjMonoid :: IO ()
> testBoolDisjMonoid = hspec $ do
>   describe "BoolDisj monoid" $ do
>     it "is associative" $ do
>       property (semigroupAssoc :: BoolDisjAssoc)
>     it "has left id" $ do
>       property
>         (monoidLeftIdentity :: BoolDisj -> Bool)
>     it "has right id" $ do
>       property
>         (monoidRightIdentity :: BoolDisj -> Bool)

6.

> instance Monoid b
>       => Monoid (Combine a b) where
>   mempty = Combine $ const mempty

> semigroupFunAssoc :: (Semigroup m, Eq a)
>                   => (m -> a) -> m -> m -> m -> Bool
> semigroupFunAssoc f a b c =
>   f ((a <> b) <> c) == f (a <> (b <> c))

> type CombineAssoc a b =
>      Combine a b
>   -> Combine a b
>   -> Combine a b
>   -> Bool

> monoidLeftFunIdentity :: (Monoid m, Eq a)
>                       => (m -> a) -> m -> Bool
> monoidLeftFunIdentity f m =
>   f m == f (mempty <> m)

> monoidRightFunIdentity :: (Monoid m, Eq a)
>                        => (m -> a) -> m -> Bool
> monoidRightFunIdentity f m =
>   f m == f (m <> mempty)

> testCombineMonoid :: IO ()
> testCombineMonoid = hspec $ do
>   describe "Monoid instance for Combine" $ do
>     it "is associative" $ do
>       property $ forAll (arbitrary :: Gen (First Int)) $
>         \i -> (semigroupFunAssoc (($ i) . unCombine)
>                :: CombineAssoc (First Int) (Last Int))
>     it "has left identity" $ do
>       property $ forAll (arbitrary :: Gen String) $
>         \i ->
>           (monoidLeftFunIdentity (($ i) . unCombine)
>            :: Combine String (First Int) -> Bool)
>     it "has right identity" $ do
>       property $ forAll (arbitrary :: Gen String) $ \s ->
>         (monoidRightFunIdentity (flip ($) s . unCombine)
>          :: Combine String (Last Integer) -> Bool)

7. Hint: We can do something that seems a little more specific and natural to functions now that the input and output types are the same.

> instance Monoid (Comp a) where
>   mempty = Comp id

> type CompAssoc a =
>   Comp a -> Comp a -> Comp a -> Bool

> testCompMonoid :: IO ()
> testCompMonoid = hspec $ do
>   describe "Monoid instance for Comp" $ do
>     it "is associative" $ do
>       property $ forAll (arbitrary :: Gen Integer) $
>         \i -> (semigroupFunAssoc (($ i) . unComp)
>                :: CompAssoc Integer)
>     it "has left identity" $ do
>       property $ forAll (arbitrary :: Gen Int) $
>         \i -> (monoidLeftFunIdentity (($ i) . unComp)
>                :: Comp Int -> Bool)
>     it "has right identity" $ do
>       property $ forAll (arbitrary :: Gen String) $
>         \s -> (monoidRightFunIdentity (($ s) . unComp)
>                :: Comp String -> Bool)

8. This next exercise will involve doing something that will feel a bit unnatural still and you may find it difficult.

> newtype Mem s a =
>   Mem {
>     runMem :: s -> (a, s)
> }

> instance Semigroup a
>       => Semigroup (Mem s a) where
>   Mem f <> Mem g = Mem $ \s ->
>      let (a1, s1) = f s
>          (a2, s2) = g s1
>      in (a1 <> a2, s2)

> instance Monoid a
>       => Monoid (Mem s a) where
>   mempty = Mem $ \s -> (mempty, s)

> instance (Arbitrary a, Arbitrary s, CoArbitrary s)
>       => Arbitrary (Mem s a) where
>   arbitrary = Mem <$> arbitrary

> instance (Show s, Read s, Show a)
>       => Show (Mem s a) where
>   show = ("Mem " ++) . show . functionShow . runMem

> type MemAssoc s a =
>   Mem s a -> Mem s a -> Mem s a -> Bool

> testMemMonoid :: IO ()
> testMemMonoid = do
>   let f' = Mem $ \s -> ("hi", s + 1)
>       rmzero = runMem mempty 0
>       rmleft = runMem (f' <> mempty) 0
>       rmright = runMem (mempty <> f') 0
>   hspec $ do
>   describe "Monoid instance for Mem" $ do
>     it "has left id example (\"hi\", 1)" $ do
>       rmleft `shouldBe` ("hi", 1)
>     it "has right id example (\"hi\", 1)" $ do
>       rmright `shouldBe` ("hi", 1)
>     it "has mempty example (\"\", 0)" $ do
>       (rmzero :: (String, Int)) `shouldBe` ("", 0)
>     it "has another left id example" $ do
>       rmleft `shouldBe` runMem f' 0
>     it "has another right id example" $ do
>       rmright `shouldBe` runMem f' 0
>     it "is associative" $ do
>       property $ forAll (arbitrary :: Gen Int) $
>         \i -> (semigroupFunAssoc (($ i) . runMem)
>                :: MemAssoc Int String)
>     it "has left identity" $ do
>       property $ forAll (arbitrary :: Gen Bool) $
>         \b -> (monoidLeftFunIdentity (($ b) . runMem)
>                :: Mem Bool (Last Int) -> Bool)
>     it "has right identity" $ do
>       property $ forAll (arbitrary :: Gen Integer) $
>         \i -> (monoidRightFunIdentity (($ i) . runMem)
>                :: Mem Integer String -> Bool)
