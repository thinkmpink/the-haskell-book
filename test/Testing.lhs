Continued Chapter Exercises for Ch 11. Testing.
Other work for this chapter can be found in:
  Other packages:
    - addition
    - arbitrary-instances
    - hangman
    - morse
  Other modules in this package:
    - WordNumberTest

> module Testing where
>
> import Test.Hspec
> import Test.QuickCheck
> import Data.List (sort)
> import Data.Char (toUpper)
>
> main :: IO ()
> main = do
>   testHalfIdentity
>   testPlus
>   testMult

Using QuickCheck

Test some basic properties using QuickCheck.
1. For a function:

> half :: (Fractional a) => a -> a
> half x = x / 2

this property should hold:

> halfIdentity :: Fractional a => a -> a
> halfIdentity = (*2) . half

> testHalfIdentity :: IO ()
> testHalfIdentity = do
>  quickCheck $ \a ->
>    halfIdentity a == (a :: Double)

2. For any list you apply sort to this property should hold.

> listOrdered :: (Ord a) => [a] -> Bool
> listOrdered xs =
>   snd $ foldr go (Nothing, True) xs
>   where go _ status @(_, False) = status
>         go y (Nothing, t) = (Just y, t)
>         go y (Just x, t) = (Just y, x >= y)
>
> listGen :: Gen [Int]
> listGen = arbitrary
>
> prop_listOrdered :: Property
> prop_listOrdered =
>   forAll listGen $ listOrdered . sort

3. Now we'll test the associative and commutative properties of addition:

> plusAssociative :: (Num a, Eq a)
>                 => a -> a -> a -> Bool
> plusAssociative x y z =
>   x + (y + z) == (x + y) + z

> plusCommutative :: (Num a, Eq a)
>                 => a -> a -> Bool
> plusCommutative x y =
>   x + y == y + x

> testPlus :: IO ()
> testPlus = hspec $ do
>   describe "plus" $ do
>     it "is associative" $ do
>       property $ (\a b c ->
>         plusAssociative (a :: Int)
>                         (b :: Int)
>                         (c :: Int))
>     it "is commutative" $ do
>       property $ (\a b ->
>         plusCommutative (a :: Int)
>                         (b :: Int))

4.  Now do the same for multiplication.

> multAssociative :: (Eq a, Num a)
>                 => a -> a -> a -> Bool
> multAssociative a b c =
>   (a * b) * c == a * (b * c)

> multCommutative :: (Eq a, Num a)
>                 => a -> a -> Bool
> multCommutative a b =
>   a * b == b * a

> testMult :: IO ()
> testMult = hspec $ do
>   describe "multiplication" $ do
>     it "is associative" $ do
>       property $ \a b c ->
>           multAssociative (a :: Int)
>                           (b :: Int)
>                           (c :: Int)
>     it "is commutative" $ do
>       property $ forAll
>         (arbitrary :: Gen (Int, Int)) $
>         uncurry multCommutative

5. We mentioned in one of the first chapters that there are some laws involving the relationship of quot and rem and div and mod. Write QuickCheck tests to prove them.

> quotRemId :: Integral a => a -> a -> Bool
> quotRemId x y =
>   (quot x y)*y + (rem x y) == x
>
> prop_quotRemIdInteger :: Integer -> Integer -> Bool
> prop_quotRemIdInteger a b
>   | b == 0    = True
>   | otherwise = quotRemId a b
>
> divModId :: Integral a => a -> a -> Bool
> divModId x y =
>   (div x y)*y + (mod x y) == x
>
> prop_divModIdInteger :: Integer -> Integer -> Bool
> prop_divModIdInteger _ 0 = True
> prop_divModIdInteger x y = divModId x y

> testIntegral :: IO ()
> testIntegral = hspec $ do
>   describe "quot and rem" $ do
>     it "the quotient times divisor \
>         \plus the remainer is the \
>         \dividend" $ do
>       property prop_quotRemIdInteger
>     it "same for div mod" $ do
>       property prop_divModIdInteger

6. Is (^) associative? Is it commutative? Use QuickCheck to see if the computer can contradict such an assertion.

> exponentAssoc :: (Eq a, Num a, Integral b)
>               => a -> b -> b -> Bool
> exponentAssoc a b1 b2 =
>   (a ^ b1) ^ b2 == a ^ (b1 ^ b2)

> testExponentAssoc :: IO ()
> testExponentAssoc = hspec $ do
>   describe "(^)" $ do
>     it "is associative" $ do
>       property $ \a b c ->
>         exponentAssoc (a :: Int)
>                       (b :: Int)
>                       (c :: Int)

(^)
  is associative FAILED [1]

Failures:

  test/Testing.lhs:146:7:
  1) (^) is associative
       Falsified (after 1 test):
         0
         0
         0

> exponentCommut :: Integral a => a -> a -> Bool
> exponentCommut a b =
>   a ^ b == b ^ a

> testExponentCommut :: IO ()
> testExponentCommut = quickCheck $ \a b ->
>   exponentCommut (a :: Integer) (b :: Integer)

*** Failed! Falsified (after 2 tests):
0
1

(^) is neither associative nor commutative.

7. Test that reversing a list twice is the same as the identity of the list

> twoReverse :: Property
> twoReverse = forAll (arbitrary :: Gen [Int]) $ \xs ->
>   (reverse . reverse) xs == id xs

8. Write a property for the definition of ($)

> functionApplicationEq :: Eq b => (a -> b) -> a -> Bool
> functionApplicationEq f a = f a == (f $ a)

> prop_funApp :: Property
> prop_funApp = forAll (arbitrary :: Gen String) $
>   functionApplicationEq length

> functionCompositionEq :: Eq c
>                       => (a -> b)
>                       -> (b -> c)
>                       -> a
>                       -> Bool
> functionCompositionEq f g a =
>   (g . f) a == g (f a)

> prop_funComp :: Property
> prop_funComp = forAll (arbitrary :: Gen Int) $
>   functionCompositionEq (+3) (*3)

9. See if these two functions are equal:
foldr (:) == (++)

They are not equal:

> foldConsEqConcat :: Eq a => [a] -> [a] -> Bool
> foldConsEqConcat as bs =
>   foldr (:) as bs == as ++ bs

> prop_foldConsEqConcat :: (Eq a, Arbitrary a, Show a)
>                       => Gen ([a], [a])
>                       -> Property
> prop_foldConsEqConcat gen =
>   forAll gen $ uncurry foldConsEqConcat

*Testing Test.QuickCheck> quickCheck $ prop_foldConsEqConcat (arbitrary :: Gen ([Int], [Int]))
*** Failed! Falsified (after 4 tests):
([-3,2,-1],[-1])

See if these two functions are equal:
foldr (++) [] == concat

These should be equal.

> foldAppendEqConcat :: Eq a => [[a]] -> Bool
> foldAppendEqConcat as =
>   foldr (++) [] as == concat as

> prop_foldAppendEqConcat :: (Arbitrary a, Eq a, Show a)
>                         => Gen [[a]]
>                         -> Property
> prop_foldAppendEqConcat gen =
>   forAll gen foldAppendEqConcat

*Testing Test.QuickCheck> quickCheck $ prop_foldAppendEqConcat (arbitrary :: Gen [String])
+++ OK, passed 100 tests.

10. Hm> Is that so?

> allListsInfinite :: Int -> [a] -> Bool
> allListsInfinite n xs = length (take n xs) == n

> prop_allListsInfinite :: (Eq a, Arbitrary a, Show a)
>                       => Gen (Int, [a])
>                       -> Property
> prop_allListsInfinite gen =
>   forAll gen $ uncurry allListsInfinite

*Testing Test.QuickCheck> quickCheck $ prop_allListsInfinite (arbitrary :: Gen (Int, String))
*** Failed! Falsified (after 2 tests):
(-1,"K")

11. Finally, this is a fun one. You may remember we had you compose read and show one time to complete a “round trip.” Well, now you can test that it works:
f x = (read (show x)) == x

> prop_readShow :: (Arbitrary a, Eq a, Read a, Show a)
>               => Gen a
>               -> Property
> prop_readShow gen =
>   forAll gen (\a -> read (show a) == a)

*Testing Test.QuickCheck> quickCheck $ prop_readShow (arbitrary :: Gen (Bool, String))
+++ OK, passed 100 tests.
*Testing Test.QuickCheck> quickCheck $ prop_readShow (arbitrary :: Gen (Either Bool String))
+++ OK, passed 100 tests.


Failure

Find out why this property fails.

> square :: Num a => a -> a
> square x = x * x

> squareIdentity :: Floating a => a -> a
> squareIdentity = square . sqrt

> prop_squareIdentity :: (Eq a, Floating a) => a -> Bool
> prop_squareIdentity a = squareIdentity a == a

*Testing Test.QuickCheck> quickCheck prop_squareIdentity
*** Failed! Falsified (after 4 tests):
-1.4908550659459072

Square root is defined for floating point precision numbers, and thus cannot accurately represent equality in many cases. Floating point numbers are approximations of the rationals or the reals, and thus depending on the sequence of numbers and the precision fail when compared for equality, where infinite precision numbers would not.


Idempotence

Use QuickCheck and the following helper functions to demonstrate idempotence for the following:

> twice :: (a -> a) -> a -> a
> twice f = f . f

> fourTimes :: (a -> a) -> a -> a
> fourTimes = twice . twice

1.

> idempotent_capitalize :: String -> Bool
> idempotent_capitalize x =
>   (capitalizeWord x == twice capitalizeWord x)
>   &&
>   (capitalizeWord x == fourTimes capitalizeWord x)
>
>   where capitalizeWord []     = []
>         capitalizeWord (a:as) = toUpper a: as

*Testing Test.QuickCheck> quickCheck idempotent_capitalize
+++ OK, passed 100 tests.

2.

> idempotent_sort :: (Eq a, Ord a) => [a] -> Bool
> idempotent_sort x =
>   (sort x == twice sort x)
>   &&
>   (sort x == fourTimes sort x)

*Testing Test.QuickCheck> quickCheck $ forAll (arbitrary :: Gen String) idempotent_sort
+++ OK, passed 100 tests.

Make a Gen random generator for the datatype
We demonstrated in the chapter how to make Gen generators for different datatypes. We are so certain you enjoyed that, we are going to ask you to do it for some new datatypes:
1. Equal probabilities for each.

> data Fool =
>     Fulse
>   | Frue
>   deriving (Eq, Show)

> foolEqProbs :: Gen Fool
> foolEqProbs = oneof [ return Fulse
>                     , return Frue
>                     ]

*Testing Test.QuickCheck> sample foolEqProbs
Fulse
Frue
Frue
Fulse
Frue
Fulse
Frue
Fulse
Frue
Fulse
Fulse

2. 2/3s chance of Fulse, 1/3 chance of Frue.

> foolTwoThirdsFulse :: Gen Fool
> foolTwoThirdsFulse = frequency [ (2, return Fulse)
>                                , (1, return Frue)
>                                ]

*Testing Test.QuickCheck> sample foolTwoThirdsFulse
Fulse
Frue
Fulse
Fulse
Fulse
Fulse
Fulse
Frue
Frue
Fulse
Fulse
