> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
>
> module AlgebraicDatatypes where
>
> import Data.Int
> import Data.Char
> import Data.List
>
> data Doggies a =
>     Husky a
>   | Mastiff a deriving (Eq, Show)
>
> data PugType = PugData
>
> data HuskyType a = HuskyData
>
> data DogueDeBordeaux doge = DogueDeBordeaux doge

Exercises: Dog Types
Given the datatypes defined in the above sections,
1. Is Doggies a type constructor or a data constructor?

Doggies is a type constructor. For a type `a`, Doggies a is a type that wraps a.

2. What is the kind of Doggies?

The kind of Doggies is * -> *.

3. What is the kind of Doggies String?

The kind of Doggies String is *.

4. What is the type of Husky 10?

Husky 10 :: Num a => Doggies a

5. What is the type of Husky (10 :: Integer)?

Husky (10 :: Integer) :: Doggies Integer

6. What is the type of Mastiff "Scooby Doo"?

Mastiff "Scooby Doo" :: Doggies String

7. Is DogueDeBordeaux a type constructor or a data constructor?

It is both.

8. What is the type of DogueDeBordeaux?

DogueDeBordeaux :: doge -> DogueDeBordeaux doge

9. What is the type of DogueDeBordeaux "doggie!"

DogueDeBordeaux "doggie!" :: DogueDeBordeaux String


> data Price =
>   Price Integer deriving (Eq, Show)

> data Size =
>   Size Double deriving (Eq, Show)

> data Manufacturer =
>   Mini | Mazda | Tata
>   deriving (Eq, Show)

> data Airline =
>     PapuAir
>   | CatapultsR'Us
>   | TakeYourChancesUnited
>     deriving (Eq, Show)

> data Vehicle = Car Manufacturer Price
>              | Plane Airline Size
>              deriving (Eq, Show)


Exercises: Vehicles

> myCar    = Car Mini (Price 14000)
> urCar    = Car Mazda (Price 20000)
> clownCar = Car Tata (Price 7000)
> doge     = Plane PapuAir

1. What is the type of myCar?

myCar :: Vehicle

2. Given the following, define the functions:

> isCar :: Vehicle -> Bool
> isCar (Car _ _) = True
> isCar _         = False

> isPlane :: Vehicle -> Bool
> isPlane (Plane _ _) = True
> isPlane _           = False

> areCars :: [Vehicle] -> [Bool]
> areCars = map isCar

3. Now we’re going to write a function to tell us the manufacturer of a piece of data:

> getManu :: Vehicle -> Manufacturer
> getManu (Car m _) = m

4. Given that we’re returning the Manufacturer, what will happen if you use this on Plane data?

We will blow up because this value is bottom.

5. All right. Let’s say you’ve decided to add the size of the plane as an argument to the Plane constructor. Add that to your datatypes in the appropriate places and change your data and functions appropriately.

Done. We don't have to add an argument to doge if we are okay with it being a function from a Size to a Vehicle.


Exercises: Cardinality
While we haven’t explicitly described the rules for calculating the cardinality of datatypes yet, you might already have an idea of how to do it for simple datatypes with nullary constructors. Try not to overthink these exercises – follow your intuition based on what you know.

1. data PugType = PugData
This type has cardinality of 1.

2. For this one, recall that Bool is also defined with the |:

data Airline =
       PapuAir
     | CatapultsR'Us
     | TakeYourChancesUnited

This has cardinality 3 (3 possible values).

3. Given what we know about Int8, what’s the cardinality of Int16?

Int16 has cardinality of C(Int16) = C(Int8) * C(Int8) = 256 * 256 = 65536.

4. Use the REPL and maxBound and minBound to examine Int and Integer. What can you say about the cardinality of those types?

mp> maxBound :: Int
9223372036854775807
mp> minBound :: Int
-9223372036854775808
mp> (fromIntegral (maxBound :: Int)) * 2 :: Integer
18446744073709551614

The cardinality of Int is therefore 18446744073709551614.

On the other hand, Integer is unbounded:

mp> maxBound :: Integer

<interactive>:31:1: error:
    • No instance for (Bounded Integer)
        arising from a use of ‘maxBound’
    • In the expression: maxBound :: Integer
      In an equation for ‘it’: it = maxBound :: Integer
mp> minBound :: Integer

<interactive>:32:1: error:
    • No instance for (Bounded Integer)
        arising from a use of ‘minBound’
    • In the expression: minBound :: Integer
      In an equation for ‘it’: it = minBound :: Integer

In other words, Integer has infinite cardinality.

5. Extra credit (impress your friends!): What’s the connection between the 8 in Int8 and that type’s cardinality of 256?

The 8 is the number of bits needed to obtain the cardinality of 256. If n bits let us represent 2^n values, then as an example:

2^8 = 256


> data Example = MakeExample deriving Show

Exercises: For Example
1. You can query the type of a value in GHCi with the :type command, also abbreviated :t.
Example:
     Prelude> :t False
     False :: Bool

What is the type of data constructor MakeExample? What happens when you request the type of Example?

The type of MakeExample is Example.
MakeExample :: Example

When we request Example's type, it says we can't take the type of a type.

mp> :t Example

<interactive>:1:1: error: Data constructor not in scope: Example

2. What if you try :info on Example in GHCi? Can you determine what type class instances are defined for the Example type using :info in GHCi?

mp> :info Example
data Example = MakeExample
  	-- Defined at AlgebraicDatatypes.lhs:168:3
instance [safe] Show Example
  -- Defined at AlgebraicDatatypes.lhs:168:39

Yes, type class instances of a type are available in GHCi.

3. Try making a new datatype like Example but with a single type argument added to MakeExample, such as Int. What has changed when you query MakeExample with :type in GHCi?

> data Example' = MakeExample' Int
>   deriving Show

MakeExample' :: Int -> Example'

It is now a function, not a value. It is a unary constructor, not a nullary one.


> class TooMany a
>   where tooMany :: a -> Bool

> instance TooMany Int
>   where tooMany n = n > 42


> newtype Goats = Goats Int deriving Show

> instance TooMany Goats where
>   tooMany (Goats n) = n > 43


Exercises: Logic Goats
1. Reusing the TooMany type class, write an instance of the type class for the type (Int, String). This will require adding a language pragma named FlexibleInstances5 if you do not use a newtype – GHC will tell you what to do.

> instance TooMany (Int, String) where
>   tooMany (i, _) = i > 42
>
> newtype IntString = IntString (Int, String)
>   deriving (Eq, Show, TooMany)

2. Make another TooMany instance for (Int, Int). Sum the values together under the assumption this is a count of goats from two fields.

> instance TooMany (Int, Int) where
>   tooMany (a, b) = a + b > 84

3. Make another TooMany instance, this time for (Num a, TooMany a) => (a, a). This can mean whatever you want, such as summing the two numbers together.

> instance (Num a, TooMany a) => TooMany (a, a) where
>   tooMany (a, b) = tooMany (a+b)


Exercises: Pity the Bool
1. Given a datatype

> data BigSmall =
>     Big Bool
>   | Small Bool
>   deriving (Eq, Show)

What is the cardinality of this datatype? Hint: We already know Bool’s cardinality. Show your work as demonstrated earlier.

We already know the cardinality of unary data constructors (id of their contents), and of sum types.

Big Bool | Small Bool = ??
Big Bool + Small Bool == ??
Bool + Bool == ??
True + False + True + False == ??
1 + 1 + 1 + 1 == 4

The cardinality of this data type is 4.

2. Given a datatype

bring Int8 in scope with import Data.Int

> data NumberOrBool =
>     Numba Int8
>   | BoolyBool Bool
>   deriving (Eq, Show)
>
> myNumba = Numba (-128)


What is the cardinality of NumberOrBool? What happens if you try to create a Numba with a numeric literal larger than 127? And with a numeric literal smaller than (-128)?

Numba Int8 | BoolyBool Bool = ??
Numba Int8 + BoolyBool Bool == ??
Int8 + Bool == ??
256 + 2 == 258 <= the cardinality of NumberOrBool.

If you try to create a Numba with a numeric literal outside of the allowed range, the compiler issues a warning and wraps the integer around modulo the size of the range.

mp> Numba 256

<interactive>:77:7: warning: [-Woverflowed-literals]
    Literal 256 is out of the GHC.Int.Int8 range -128..127
Numba 0


> jm = Person "julie" 108
> ca = Person "chris" 16

> data Person =
>   Person { name :: String
>          , age :: Int
>          }

> type AuthorName = String

> data Author =
>     Fiction AuthorName
>   | Nonfiction AuthorName
>   deriving (Eq, Show)

Exercises: How Does Your Garden Grow?

1. Given the type
data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String
data Garden =
  Garden Gardener FlowerType
  deriving Show

What is the sum of products normal form of Garden?

> type Gardener = String
> data Garden = Gardenia Gardener
>             | Daisy Gardener
>             | Rose Gardener
>             | Lilac Gardener
>             deriving Show

> data GuessWhat =
>   Chickenbutt deriving (Eq, Show)
>
> data Id a = MkId a deriving (Eq, Show)

> data Product a b = Product a b
>    deriving (Eq, Show)

> data Sum a b =
>     First a
>   | Second b
>   deriving (Eq, Show)

> data RecordProduct a b =
>   RecordProduct { pfirst :: a
>                 , psecond :: b }
>                 deriving (Eq, Show)

> newtype NumCow =
>   NumCow Int
>   deriving (Eq, Show)

> newtype NumPig =
>   NumPig Int
>   deriving (Eq, Show)

> data Farmhouse =
>   Farmhouse NumCow NumPig
>   deriving (Eq, Show)

> type Farmhouse' = Product NumCow NumPig

> newtype NumSheep =
>   NumSheep Int
>   deriving (Eq, Show)

> data BigFarmhouse =
>   BigFarmhouse NumCow NumPig NumSheep
>   deriving (Eq, Show)

> type BigFarmhouse' =
>   Product NumCow (Product NumPig NumSheep)

> type Age = Int
> type Name = String
> type LovesMud = Bool
> type PoundsOfWool = Int

> data CowInfo =
>   CowInfo Name Age
>   deriving (Eq, Show)

> data PigInfo =
>   PigInfo Name Age LovesMud
>   deriving (Eq, Show)

> data SheepInfo =
>   SheepInfo Name Age PoundsOfWool
>   deriving (Eq, Show)

> data Animal =
>     Cow CowInfo
>   | Pig PigInfo
>   | Sheep SheepInfo
>   deriving (Eq, Show)

> type Animal' =
>   Sum CowInfo (Sum PigInfo SheepInfo)

> trivialValue :: GuessWhat
> trivialValue = Chickenbutt

> idInt :: Id Integer
> idInt = MkId 10

> type Awesome = Bool

> person :: Product Name Awesome
> person = Product "Simon" True

> data Twitter =
>   Twitter deriving (Eq, Show)

> data AskFm =
>   AskFm deriving (Eq, Show)

> socialNetwork :: Sum Twitter AskFm
> socialNetwork = First Twitter

> data OperatingSystem =
>     GnuPlusLinux
>   | OpenBSDPlusNevermindJustBSDStill
>   | Mac
>   | Windows
>   deriving (Eq, Show)

> data ProgLang =
>        Haskell
>      | Agda
>      | Idris
>      | PureScript
>      deriving (Eq, Show)

> data Programmer =
>   Programmer { os :: OperatingSystem
>              , lang :: ProgLang }
>   deriving (Eq, Show)

> nineToFive :: Programmer
> nineToFive = Programmer { os = Mac
>                         , lang = Haskell }

> feelingWizardly :: Programmer
> feelingWizardly =
>   Programmer { lang = Agda
>              , os = GnuPlusLinux }

Exercise: Programmers
Write a function that generates all possible values of Programmer. Use the provided lists of inhabitants of OperatingSystem and ProgLang.

> allOperatingSystems :: [OperatingSystem]
> allOperatingSystems =
>   [ GnuPlusLinux
>   , OpenBSDPlusNevermindJustBSDStill
>   , Mac
>   , Windows
>   ]

> allLanguages :: [ProgLang]
> allLanguages =
>   [Haskell, Agda, Idris, PureScript]

> allProgrammers :: [Programmer]
> allProgrammers =
>   [ Programmer os lang |
>     os <- allOperatingSystems
>   , lang <- allLanguages
>   ]

> newtype Name' = Name' String deriving Show
> newtype Acres = Acres Int deriving Show

> data FarmerType = DairyFarmer
>                 | WheatFarmer
>                 | SoybeanFarmer
>                 deriving Show

> data Farmer =
>   Farmer Name' Acres FarmerType
>   deriving Show

> isDairyFarmer :: Farmer -> Bool
> isDairyFarmer (Farmer _ _ DairyFarmer) =
>   True
> isDairyFarmer _ =
>   False

Let’s review the arithmetic of sum types:

> data Quantum =
>     Yes
>   | No
>   | Both
>   deriving (Eq, Show)

> quantSum1 :: Either Quantum Quantum
> quantSum1 = Right Yes

> quantSum2 :: Either Quantum Quantum
> quantSum2 = Right No

> quantSum3 :: Either Quantum Quantum
> quantSum3 = Right Both

> quantSum4 :: Either Quantum Quantum
> quantSum4 = Left Yes

> quantSum5 :: Either Quantum Quantum
> quantSum5 = Left No

> quantSum6 :: Either Quantum Quantum
> quantSum6 = Left Both

And now the arithmetic of product types:

> quantProd1 :: (Quantum, Quantum)
> quantProd1 = (Yes, Yes)

> quantProd2 :: (Quantum, Quantum)
> quantProd2 = (Yes, No)

> quantProd3 :: (Quantum, Quantum)
> quantProd3 = (Yes, Both)

> quantProd4 :: (Quantum, Quantum)
> quantProd4 = (No, Yes)

> quantProd5 :: (Quantum, Quantum)
> quantProd5 = (No, No)

> quantProd6 :: (Quantum, Quantum)
> quantProd6 = (No, Both)

> quantProd7 :: (Quantum, Quantum)
> quantProd7 = (Both, Yes)

> quantProd8 :: (Quantum, Quantum)
> quantProd8 = (Both, No)

> quantProd9 :: (Quantum, Quantum)
> quantProd9 = (Both, Both)

And now a function type. Each possible unique implementation of the function is an inhabitant:

> quantFlip1 :: Quantum -> Quantum
> quantFlip1 Yes  = Yes
> quantFlip1 No   = Yes
> quantFlip1 Both = Yes

> quantFlip2 :: Quantum -> Quantum
> quantFlip2 Yes  = Yes
> quantFlip2 No   = Yes
> quantFlip2 Both = No

> quantFlip3 :: Quantum -> Quantum
> quantFlip3 Yes  = Yes
> quantFlip3 No   = Yes
> quantFlip3 Both = Both

> quantFlip4 :: Quantum -> Quantum
> quantFlip4 Yes  = Yes
> quantFlip4 No   = No
> quantFlip4 Both = Yes

> quantFlip5 :: Quantum -> Quantum
> quantFlip5 Yes  = Yes
> quantFlip5 No   = Both
> quantFlip5 Both = Yes

> quantFlip6 :: Quantum -> Quantum
> quantFlip6 Yes  = No
> quantFlip6 No   = Yes
> quantFlip6 Both = Yes

> quantFlip7 :: Quantum -> Quantum
> quantFlip7 Yes  = Both
> quantFlip7 No   = Yes
> quantFlip7 Both = Yes

> quantFlip8 :: Quantum -> Quantum
> quantFlip8 Yes  = No
> quantFlip8 No   = No
> quantFlip8 Both = No

> quantFlip9 :: Quantum -> Quantum
> quantFlip9 Yes  = No
> quantFlip9 No   = No
> quantFlip9 Both = Yes

> quantFlip10 :: Quantum -> Quantum
> quantFlip10 Yes  = No
> quantFlip10 No   = No
> quantFlip10 Both = Both

> quantFlip11 :: Quantum -> Quantum
> quantFlip11 Yes  = No
> quantFlip11 No   = Yes
> quantFlip11 Both = No

> quantFlip12 :: Quantum -> Quantum
> quantFlip12 Yes  = No
> quantFlip12 No   = Both
> quantFlip12 Both = No

> quantFlip13 :: Quantum -> Quantum
> quantFlip13 Yes  = Yes
> quantFlip13 No   = No
> quantFlip13 Both = No

> quantFlip14 :: Quantum -> Quantum
> quantFlip14 Yes  = Both
> quantFlip14 No   = No
> quantFlip14 Both = No

> quantFlip15 :: Quantum -> Quantum
> quantFlip15 Yes  = Both
> quantFlip15 No   = Both
> quantFlip15 Both = Both

> quantFlip16 :: Quantum -> Quantum
> quantFlip16 Yes  = Both
> quantFlip16 No   = Both
> quantFlip16 Both = Yes

> quantFlip17 :: Quantum -> Quantum
> quantFlip17 Yes  = Both
> quantFlip17 No   = Both
> quantFlip17 Both = No

> quantFlip18 :: Quantum -> Quantum
> quantFlip18 Yes  = Both
> quantFlip18 No   = Yes
> quantFlip18 Both = Both

> quantFlip19 :: Quantum -> Quantum
> quantFlip19 Yes  = Both
> quantFlip19 No   = No
> quantFlip19 Both = Both

> quantFlip20 :: Quantum -> Quantum
> quantFlip20 Yes  = Yes
> quantFlip20 No   = Both
> quantFlip20 Both = Both

> quantFlip21 :: Quantum -> Quantum
> quantFlip21 Yes  = No
> quantFlip21 No   = Both
> quantFlip21 Both = Both

> quantFlip22 :: Quantum -> Quantum
> quantFlip22 Yes  = Yes
> quantFlip22 No   = No
> quantFlip22 Both = Both

> quantFlip23 :: Quantum -> Quantum
> quantFlip23 Yes  = Both
> quantFlip23 No   = Yes
> quantFlip23 Both = No

> quantFlip24 :: Quantum -> Quantum
> quantFlip24 Yes  = No
> quantFlip24 No   = Both
> quantFlip24 Both = Yes

> quantFlip25 :: Quantum -> Quantum
> quantFlip25 Yes  = Yes
> quantFlip25 No   = Both
> quantFlip25 Both = No

> quantFlip26 :: Quantum -> Quantum
> quantFlip26 Yes  = No
> quantFlip26 No   = Yes
> quantFlip26 Both = Both

> quantFlip27 :: Quantum -> Quantum
> quantFlip27 Yes  = Both
> quantFlip27 No   = No
> quantFlip27 Both = Yes

Exponentiation in what order?
Consider the following function:

convert :: Quantum -> Bool
convert = undefined

According to the equality of a -> b and 𝑏𝑎 there should be 23 or 8 implementations of this function. Does this hold? Write it out and prove it for yourself.

> convertQB1 :: Quantum -> Bool
> convertQB1 Yes  = True
> convertQB1 No   = True
> convertQB1 Both = True

> convertQB2 :: Quantum -> Bool
> convertQB2 Yes  = True
> convertQB2 No   = True
> convertQB2 Both = False

> convertQB3 :: Quantum -> Bool
> convertQB3 Yes  = True
> convertQB3 No   = False
> convertQB3 Both = True

> convertQB4 :: Quantum -> Bool
> convertQB4 Yes  = False
> convertQB4 No   = True
> convertQB4 Both = True

> convertQB5 :: Quantum -> Bool
> convertQB5 Yes  = False
> convertQB5 No   = False
> convertQB5 Both = False

> convertQB6 :: Quantum -> Bool
> convertQB6 Yes  = False
> convertQB6 No   = False
> convertQB6 Both = True

> convertQB7 :: Quantum -> Bool
> convertQB7 Yes  = False
> convertQB7 No   = True
> convertQB7 Both = False

> convertQB8 :: Quantum -> Bool
> convertQB8 Yes  = True
> convertQB8 No   = False
> convertQB8 Both = False


Exercises: The Quad
Determine how many unique inhabitants each type has. Suggestion: do the arithmetic unless you want to verify. Writing them out gets tedious quickly.

1.

> data Quad =
>     One
>   | Two
>   | Three
>   | Four
>   deriving (Eq, Show)

-- how many different forms can this take?
eQuad :: Either Quad Quad
eQuad = ???

Since Quad has 4 values, Either Quad Quad has 4 + 4 == 8 values.

2. prodQuad :: (Quad, Quad)

This has 4 * 4 == 16 values.

3. funcQuad :: Quad -> Quad

This has 4 ^ 4 == 256 values

4. prodTBool :: (Bool, Bool, Bool)

This has 2 * 2 * 2 == 8 values.

5. gTwo :: Bool -> Bool -> Bool

This has (2 ^ 2) ^ 2 == 4 ^ 2 == 16 values.

6. Hint: 5 digit number
     fTwo :: Bool -> Quad -> Quad

This has (4 ^ 4) ^ 2 == 65536 values.


11.17 Binary Tree

> data BinaryTree a =
>     Leaf
>   | Node (BinaryTree a) a (BinaryTree a)
>   deriving (Eq, Ord, Show)


> insert' :: Ord a
>         => a
>         -> BinaryTree a
>         -> BinaryTree a
> insert' b Leaf = Node Leaf b Leaf
> insert' b (Node left a right)
>   | b == a = Node left a right
>   | b < a  = Node (insert' b left) a right
>   | b > a  = Node left a (insert' b right)

Exercises.

Write map for BinaryTree.

> mapTree :: (a -> b)
>         -> BinaryTree a
>         -> BinaryTree b
> mapTree _ Leaf = Leaf
> mapTree f (Node left a right) =
>   Node (mapTree f left)
>        (f a)
>        (mapTree f right)
>
> testTree' :: BinaryTree Integer
> testTree' =
>   Node (Node Leaf 3 Leaf)
>   1
>   (Node Leaf 4 Leaf)
>
> mapExpected =
>   Node (Node Leaf 4 Leaf)
>   2
>   (Node Leaf 5 Leaf)
>
> -- acceptance test for mapTree
> mapOkay =
>   if mapTree (+1) testTree' == mapExpected
>   then print "yup okay!"
>   else error "test failed!"


Convert binary trees to lists.


> preorder :: BinaryTree a -> [a]
> preorder Leaf = []
> preorder (Node left a right) =
>   a: preorder left ++ preorder right
>
> inorder :: BinaryTree a -> [a]
> inorder Leaf = []
> inorder (Node left a right) =
>   inorder left ++ [a] ++ inorder right
>
> postorder :: BinaryTree a -> [a]
> postorder Leaf = []
> postorder (Node left a right) =
>   postorder left ++ postorder right ++ [a]
>
> testTree :: BinaryTree Integer
> testTree =
>   Node (Node Leaf 1 Leaf)
>        2
>        (Node Leaf 3 Leaf)
>
> testPreorder :: IO ()
> testPreorder =
>   if preorder testTree == [2, 1, 3]
>   then putStrLn "Preorder fine!"
>   else putStrLn "Bad news bears."
>
> testInorder :: IO ()
> testInorder =
>   if inorder testTree == [1, 2, 3]
>   then putStrLn "Inorder fine!"
>   else putStrLn "Bad news bears."
>
> testPostorder :: IO ()
> testPostorder =
>   if postorder testTree == [1, 3, 2]
>   then putStrLn "Postorder fine!"
>   else putStrLn "postorder failed check"
>
> main :: IO ()
> main = do
>   testPreorder
>   testInorder
>   testPostorder


Write foldr for BinaryTree.
Given the definition of BinaryTree we have provided, write a catamorphism for the binary trees.

> foldTree :: (a -> b -> b)
>          -> b
>          -> BinaryTree a
>          -> b
> foldTree _ b Leaf = b
> foldTree f b (Node left a right) =
>   foldTree
>     f
>     (foldTree f (f a b) right)
>     left

Here's another implementation to compare to.

> foldTree' f b = foldr f b . postorder


11.18 Chapter Exercises

Multiple choice

1. Given the following datatype:

> data Weekday =
>     Monday
>   | Tuesday
>   | Wednesday
>   | Thursday
>   | Friday

we can say:
a) Weekday is a type with five data constructors b) Weekday is a tree with five branches
c) Weekday is a product type
d) Weekday takes five arguments

We can only say a).

2. and with the same datatype definition in mind, what is the type of the following function, f?

f Friday = "Miller Time"

a) f :: [Char]
b) f :: String -> String
c) f :: Weekday -> String
d) f :: Day -> Beer

c) is correct

3. Types defined with the data keyword
a) must have at least one argument
b) must begin with a capital letter
c) must be polymorphic
d) cannot be imported from modules

Only b) is correct. Types do not have to have arguments or be polymorphic.

4. The function g xs = xs !! (length xs - 1)
a) is recursive and may not terminate
b) delivers the head of xs
c) delivers the final element of xs
d) has the same type as xs

c) delivers the final element of xs


Ciphers
In the Lists chapter, you wrote a Caesar cipher. Now, we want to expand on that idea by writing a Vigenère cipher. A Vigenère cipher is another substitution cipher, based on a Caesar cipher, but it uses a series of Caesar ciphers for polyalphabetic substitution. The substitution for each letter in the plaintext is determined by a fixed keyword.

see Cipher.lhs


Use as-patterns in implementing the following functions:
1. This should return True if (and only if) all the values in the first list appear in the second list, though they need not be contiguous.

> isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
> isSubseqOf []         _      = True
> isSubseqOf _          []     = False
> isSubseqOf as'@(a:as) (b:bs)
>   | a == b    = isSubseqOf as bs
>   | otherwise = isSubseqOf as' bs


The following are examples of how this function should work:
Prelude> isSubseqOf "blah" "blahwoot"
True
Prelude> isSubseqOf "blah" "wootblah"
True
Prelude> isSubseqOf "blah" "wboloath"
True
Prelude> isSubseqOf "blah" "wootbla"
False
Prelude> isSubseqOf "blah" "halbwoot"
False
Prelude> isSubseqOf "blah" "blawhoot"
True

2. Split a sentence into words, then tuple each word with the capitalized form of each.

> capitalizeWords :: String
>                 -> [(String, String)]
> capitalizeWords = map toCaps . words
>   where
>     toCaps word@(w:ws) = (word, toUpper w:ws)

Prelude> capitalizeWords "hello world"
[("hello", "Hello"), ("world", "World")]


Language exercises
1. Write a function that capitalizes a word.

> capitalizeWord :: String -> String
> capitalizeWord [] = []
> capitalizeWord (a:as) = toUpper a:as

Example output.
Prelude> capitalizeWord "Chortle"
"Chortle"
Prelude> capitalizeWord "chortle"
"Chortle"

2. Write a function that capitalizes sentences in a paragraph. Recognize when a new sentence has begun by checking for periods. Reuse the capitalizeWord function.

> capitalizeParagraph :: String -> String
> capitalizeParagraph = unsent
>                     . map capSent
>                     . sent
>   where
>     sent :: String -> [String]
>     sent [] = []
>     sent s =
>         takeWhile (/= '.') s
>       : (sent . tail . dropWhile (/= '.') $ s)
>
>     capSent :: String -> String
>     capSent = unwords . capHead . words
>
>     capHead :: [String] -> [String]
>     capHead []     = []
>     capHead (a:as) = capitalizeWord a: as
>
>     unsent :: [String] -> String
>     unsent [] = ""
>     unsent as = intercalate ". " as ++ "."

Example result you should get from your function:
Prelude> s = "blah. woot ha."
Prelude> capitalizeParagraph s
"Blah. Woot ha."


Phone exercise
Remember old-fashioned phone inputs for writing text where you had to press a button multiple times to get different letters to come up? You may still have to do this when you try to search for a movie to watch using your television remote control. You’re going to write code to translate sequences of button presses into strings and vice versa.

-----------------------------------------
|1     |2 ABC |3 DEF |
_________________________________________
|4 GHI |5 JKL |6 MNO |
-----------------------------------------
|7 PQRS|8 TUV |9 WXYZ |
-----------------------------------------
|*^    |0+_   |#., |
-----------------------------------------
Where star (*) gives you capitalization of the letter you’re writing to your friends, and 0 is your space bar. To represent the digit itself, you press that digit once more than the letters it represents. If you press a button one more than is required to type the digit, it wraps around to the first letter. For example,
 2     -> 'A'
 22    -> 'B'
 222   -> 'C'
 2222  -> '2'
 22222 -> 'A'


 1. Create a data structure that captures the phone layout above. The data structure should be able to express enough of how the layout works that you can use it to dictate the behavior of the functions in the following exercises.

> -- fill in the rest.
> data DaPhone = DaPhone [Key]
>   deriving (Show)
>
> newtype PhoneDigit = PhoneDigit Int
>   deriving (Eq, Show, Ord, Enum)
>
> instance Bounded PhoneDigit where
>   minBound = PhoneDigit 0
>   maxBound = PhoneDigit 9
>
>
> data Key =
>     SingleKey PhoneDigit
>   | NumAlphaKey PhoneDigit String
>   | Star String
>   | Pound String
>   deriving (Eq, Show)
>
> myPhone :: DaPhone
> myPhone = DaPhone
>   [ SingleKey (PhoneDigit 1)
>   , NumAlphaKey (PhoneDigit 2) "ABC"
>   , NumAlphaKey (PhoneDigit 3) "DEF"
>   , NumAlphaKey (PhoneDigit 4) "GHI"
>   , NumAlphaKey (PhoneDigit 5) "JKL"
>   , NumAlphaKey (PhoneDigit 6) "MNO"
>   , NumAlphaKey (PhoneDigit 7) "PQRS"
>   , NumAlphaKey (PhoneDigit 8) "TUV"
>   , NumAlphaKey (PhoneDigit 9) "WXYZ"
>   , Star "^"
>   , NumAlphaKey (PhoneDigit 0) "+ _"
>   , Pound ".,"
>   ]
>
> phoneDigit :: PhoneDigit -> Digit
> phoneDigit (PhoneDigit d) = head $ show d
>
> keyDigit :: Key -> Digit
> keyDigit (SingleKey p)     = phoneDigit p
> keyDigit (NumAlphaKey p _) = phoneDigit p
> keyDigit (Star _)          = '*'
> keyDigit (Pound _)         = '#'
>
> keyChars :: Key -> String
> keyChars (SingleKey p)     = [phoneDigit p]
> keyChars (NumAlphaKey p s) = s ++ [phoneDigit p]
> keyChars st@(Star s)       = s ++ [keyDigit st]
> keyChars po@(Pound s)      = s ++ [keyDigit po]
>
> hasChar :: Key -> Char -> Bool
> hasChar k c = elem (toUpper c) $ keyChars k
>


2. Convert the following conversations into the keypresses required to express them. We’re going to suggest types and functions to fill in order to accomplish the goal, but they’re not obligatory. If you want to do it differently, go right ahead.


> convo :: [String]
> convo =
>   [ "Wanna play 20 questions",
>    "Ya",
>    "U 1st haha",
>    "Lol ok. Have u ever tasted alcohol",
>    "Lol ya",
>    "Wow ur cool haha. Ur turn",
>    "Ok. Do u think I am pretty Lol",
>    "Lol ya",
>    "Just making sure rofl ur turn"]


> -- validButtons = "1234567890*#"
> type Digit = Char
>
> -- Valid presses: 1 and up
> type Presses = Int
>
> reverseTaps :: DaPhone
>             -> Char
>             -> [(Digit, Presses)]
> reverseTaps d@(DaPhone keys) ch
>   | isUpper ch = ('*', 1):reverseTaps d (toLower ch)
>   | otherwise  = [(keyDigit key, numPresses key ch)]
>   where
>     key = head . filter (flip hasChar ch) $ keys
>     numPresses k c =
>       maybe (-1) (+1) (elemIndex (toUpper c) $ keyChars k)
>
> -- assuming the default phone definition
> -- 'a' -> [('2', 1)]
> -- 'A' -> [('*', 1), ('2', 1)]
>
> cellPhonesDead :: DaPhone
>                -> String
>                -> [(Digit, Presses)]
> cellPhonesDead p = concat . map (reverseTaps p)
>

The following represents the button presses required for the whole sample conversation provided.

> convertConvo :: [[(Digit, Presses)]]
> convertConvo = map (cellPhonesDead myPhone) convo

3. How many times do digits need to be pressed for each message?

> fingerTaps :: [(Digit, Presses)] -> Presses
> fingerTaps = sum . map snd

mp> map fingerTaps convertConvo
[53,5,19,77,16,54,65,16,65]

4. What was the most popular letter for each message? What was its cost? You’ll want to combine reverseTaps and fingerTaps to figure out what it cost in taps. reverseTaps is a list because you need to press a different button in order to get capitals.

> mostPopular :: Eq a => [a] -> a
> mostPopular =
>     head
>   . maximumBy (\a b -> compare (length a) (length b))
>   . group
>
> mostPopularLetter :: String -> Char
> mostPopularLetter = mostPopular
>
> cost :: String -> Presses
> cost s = fingerTaps
>        . cellPhonesDead myPhone
>        . filter (==m) $ s
>   where m = mostPopularLetter s

mp> map cost convo
[6,1,2,9,1,9,3,1,4]

5. What was the most popular letter overall? What was the most popular word?

> coolestLtr :: [String] -> Char
> coolestLtr = mostPopularLetter . concat

mp> coolestLtr convo
't'

> coolestWord :: [String] -> String
> coolestWord = mostPopular . concat . map words
