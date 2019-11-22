
> module TypeClasses where
>
> import Data.List

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


> data DayOfWeek =
>   Mon | Tue | Weds | Thu | Fri | Sat | Sun
>   deriving (Eq, Ord, Show)
>

Exercises: Will They Work?
Next, take a look at the following code examples and try to decide if they will work, what result they will return if they do, and why or why not (be sure, as always, to test them in your REPL once you have decided on your answer):
1. max (length [1, 2, 3])
       (length [8, 9, 10, 11, 12])

This will work because length returns an Int and Int is an instance of Ord. It will return 5.

2. compare (3 * 4) (3 * 5)

This will work because the values being compared are Nums. They will be defaulted to Integer because of the defaulting rules. The output will be LT.

3. compare "Julie" True

This will not work because the two values being compared are not of the same concrete type, which is a requirement of compare.

4. (5 + 3) > (3 + 6)

This will work (intuitively) and because (5 + 3) > (3 + 6) will have the sums evaluated to the Num type class defaulted to Integer, which has an Ord instance. It evaluates to False.



6.14 Chapter Exercises
Multiple choice

1. The Eq class
a) includes all types in Haskell
b) is the same as the Ord class
c) makes equality tests possible
d) only includes numeric types

c) It makes equality tests possible. Not all types have equality of terms, and not all equatable terms are numbers.

2. The type class Ord
a) allows any two values to be compared
b) is a subclass of Eq
c) is a superclass of Eq
d) has no instance for Bool

b) Ord is a subclass of Eq. You have to have an Eq instance for a type to order its terms.

3. Suppose the type class Ord has an operator >. What is the type of >?
a) Ord a => a -> a -> Bool
b) Ord a => Int -> Bool
c) Ord a => a -> Char
d) Ord a => Char -> [Char]

a) is correct.

4. In x = divMod 16 12
a) the type of 𝑥 is Integer
b) the value of 𝑥 is undecidable
c) the type of 𝑥 is a tuple
d) 𝑥 is equal to 12 / 16

c) is correct.

5. The type class Integral includes
a) Int and Integer numbers
b) integral, real, and fractional numbers
c) Schrodinger’s cat
d) only positive numbers

a) is correct.


Does it typecheck?
For this section of exercises, you’ll be practicing looking for type and type class errors.

Examine the following code and decide whether it will typecheck. Then load it in GHCi and see if you were correct. If it doesn’t type- check, try to match the type error against your understanding of why it didn’t work. If you can, fix the error and re-run the code.


1. Does the following code typecheck? If not, why not?

data Person = Person Bool

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

No, this won't typecheck because the Person data type lacks a Show instance. We can add one trivially with `deriving`.

> data Person = Person Bool
>   deriving (Show)

> printPerson :: Person -> IO ()
> printPerson person = putStrLn (show person)

∆ printPerson (Person True)
Person True

2. Does the following typecheck? If not, why not?

data Mood = Blah
          | Woot deriving Show

settleDown x = if x == Woot
                 then Blah
                 else x

This won't typecheck because Mood requires an instance of Eq for settleDown to work.

> data Mood = Blah
>           | Woot deriving (Eq, Show)

> settleDown x = if x == Woot
>                  then Blah
>                  else x

3. If you were able to get settleDown to typecheck:
a) What values are acceptable inputs to that function?

The parameter x must be some term of type Mood.

b) What will happen if you try to run settleDown 9? Why?

You will get a type error that there is no instance of Num for the Mood type. 9 is interpreted as a Num literal, so the typechecker would expect that settleDown, which takes a Mood argument, would somehow convert the 9 Num literal to a Mood, for which it needs the Num instance.

c) What will happen if you try to run Blah > Woot? Why?

This will not typecheck because we don't have an Ord instance defined for Mood, on which > depends.

4. Does the following typecheck? If not, why not?

> type Subject = String
> type Verb = String
> type Object = String

> data Sentence =
>   Sentence Subject Verb Object
>   deriving (Eq, Show)

> s1 = Sentence "dogs" "drool"
> s2 = Sentence "Julie" "loves" "dogs"

Yes, this typechecks.


Given a datatype declaration, what can we do?
Given the following datatype definitions:

> data Rocks =
>   Rocks String deriving (Eq, Show)

> data Yeah =
>   Yeah Bool deriving (Eq, Show)

> data Papu =
>   Papu Rocks Yeah
>   deriving (Eq, Show)

Which of the following will typecheck? For the ones that don’t typecheck, why don’t they?

1. phew = Papu "chases" True

This will not typecheck. "chases" and True need to be passed in as arguments to the Rocks and Yeah data type value constructors because Papu only accepts arguments with those types.

2.

> truth = Papu (Rocks "chomskydoz")
>              (Yeah True)

This will typecheck fine.

3.

> equalityForall :: Papu -> Papu -> Bool
> equalityForall p p' = p == p'

This will typecheck too.

4. comparePapus :: Papu -> Papu -> Bool comparePapus p p' = p > p'

This will not typecheck because there is no Ord instance for Papu, which is where > is defined.


Match the types
We’re going to give you two types and their implementations. Then we’re going to ask you if you can substitute the second type for the first. You can test this by typing the first declaration and its type into a file and editing in the new one, loading to see if it fails. Don’t guess, test all your answers!


1.
For the following definition.
  a)

> i :: Num a => a
> i = 1

  b) Try replacing the type signature with the following:

i :: a

  After you’ve formulated your own answer, test that answer. Use GHCi to check what type GHC infers for the definitions we provide without a type assigned. For this exercise, you’d type in:
Prelude> i = 1
    Prelude> :t i
    -- Result elided intentionally.


The second type cannot be substituted for the first in this case. We cannot get more polymorphic types from less polymorphic ones: we cannot get the fully polymorphic a from the constrained polymorphic Num a => a.

∆ :r
[16 of 16] Compiling TypeClasses      ( TypeClasses.lhs, interpreted )

TypeClasses.lhs:300:7: error:
    • No instance for (Num a) arising from the literal ‘1’
      Possible fix:
        add (Num a) to the context of
          the type signature for:
            i :: forall a. a
    • In the expression: 1
      In an equation for ‘i’: i = 1
    |
300 | > i = 1
    |       ^
Failed, 15 modules loaded.

2.

a)

> f :: Float
> f = 1.0

b)

f :: Num a => a

The latter cannot be substituted here. f is only constrained (because it is a literal) to Fractional a => a, i.e. it need not be a Float, but it is more constrained than a Num.

3.
a)
f1 :: Float

> f1 = 1.0

b)

> f1 :: Fractional a => a

This should work, as described in 2.

4. Hint for the following: type :info RealFrac in your REPL.
a)
f2 :: Float

> f2 = 1.0

b)

> f2 :: RealFrac a => a

This should be able to be substituted, because there are instances of RealFrac for both Float and Double, which 1.0 could be read as.

5.
a)
freud :: a -> a

> freud x = x

b)

> freud :: Ord a => a  -> a

This is doable, although it's not clear why you'd want to constrain the function this way.

6. a)
freud' :: a -> a

> freud' x = x

b)

> freud' :: Int -> Int

This works. More polymorphic implementations can always be specialized to valid concrete types.

7. a)

> myX = 1 :: Int
> sigmund :: Int -> Int
> sigmund x = myX

b)

sigmund :: a -> a

This replacement cannot be done. We cannot say this is fully polymorphic when it is only defined for Int output.

8. a) myX = 1 :: Int

> sigmund' :: Int -> Int
> sigmund' x = myX

b)

sigmund' :: Num a => a -> a

Even this will not work. You cannot call this function (sigmund') with a Double.

9.
a) You’ll need to import sort from Data.List.

jung :: Ord a => [a] -> a

> jung xs = head (sort xs)

b)

> jung :: [Int] -> Int

This will work because we are turning a constrained polymorphic type signature to use valid concrete types.

10.
a) young :: [Char] -> Char

> young xs = head (sort xs)

b)

> young :: Ord a => [a] -> a

This will work. sort only depends on an Ord instance to work.

11. a)

> mySort :: [Char] -> [Char]
> mySort = sort

> signifier :: [Char] -> Char
> signifier xs = head (mySort xs)

b) signifier :: Ord a => [a] -> a

This will not work because signifier's signature has already been bound in it's implementation to Char, and not some arbitrary instance of Ord.


Type-Kwon-Do Two: Electric Typealoo

Round Two! Same rules apply – you’re trying to fill in terms (code) which’ll fit the type. The idea with these exercises is that you’ll derive the implementation from the type information. You’ll probably need to use stuff from Prelude.

See TypeKwonDo2.hs
