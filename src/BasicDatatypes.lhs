Chapter 4

> module BasicDatatypes where


Exercises: Mood Swing
Given the following datatype, answer the following questions:

> data Mood = Blah | Woot deriving Show

The deriving Show part is not something we’ve explained yet. For now, all we’ll say is that when you make your own datatypes, deriving Show allows the values of that type to be printed to the screen. We’ll talk about it more when we talk about type classes in detail.

1. What is the type constructor, or name of this type?
Mood

2. If the function requires a Mood value, what are the values you could possibly use?
Blah or Woot

3. We are trying to write a function changeMood to change Chris’s mood instantaneously. It should act like not in that, given one value, it returns the other value of the same type. So far, we’ve written a type signature changeMood :: Mood -> Woot. What’s wrong with that?

Woot is a term-level value, not a type. So Woot is not a valid part of a type signature.

4. Now we want to write the function that changes his mood. Given an input mood, it gives us the other one. Fix any mistakes and complete the function:

> changeMood :: Mood -> Mood
> changeMood Blah = Woot
> changeMood    _ = Blah

We’re doing something here called pattern matching. We can define a function by matching on a data constructor, or value, and describing the behavior that the function should have based on which value it matches. The underscore in the second line denotes a catch-all, otherwise case. So, in the first line of the function, we’re telling it what to do in the case of a specific input. In the second one, we’re telling it what to do regardless of all potential inputs. It’s trivial when there are only two potential values of a given type, but as we deal with more complex cases, it can be convenient.

5. Enter all of the above – datatype (including the deriving Show bit), your corrected type signature, and the corrected function into a source file. Load it and run it in GHCi to make sure you got it right.



Exercises: Find the Mistakes
The following lines of code may have mistakes – some of them won’t compile! You know what you need to do.
1. not True && true
2. not (x = 6)
3. (1 * 2) > 5
4. [Merry] > [Happy]
5. [1, 2, 3] ++ "look at me!"

mp> not True && true -- doesn't work

<interactive>:155:13: error:
    • Variable not in scope: true :: Bool
    • Perhaps you meant data constructor ‘True’ (imported from Prelude)
mp> not True && True -- fine
False
mp> not (x = 6) -- can't do assignment here

<interactive>:157:8: error:
    parse error on input ‘=’
    Perhaps you need a 'let' in a 'do' block?
    e.g. 'let x = 5' instead of 'x = 5'
mp> not (x == 6)
True
mp> (1 * 2) > 5
False
mp> [Merry] > [Happy] -- would need either a data type or a String

<interactive>:160:2: error:
    Data constructor not in scope: Merry :: ()

<interactive>:160:12: error:
    Data constructor not in scope: Happy :: ()
mp> ["Merry"] > ["Happy"]
True
mp> [1,2,3] ++ "look at me!" -- can't ++ [Num] String

<interactive>:163:2: error:
    • No instance for (Num Char) arising from the literal ‘1’
    • In the expression: 1
      In the first argument of ‘(++)’, namely ‘[1, 2, 3]’
      In the expression: [1, 2, 3] ++ "look at me!"
mp> "1, 2, 3" ++ "look at me!"
"1, 2, 3look at me!"



Chapter Exercises


> awesome = ["Papuchon", "curry", ":)"]
> also = ["Quake", "The Simons"]
> allAwesome = [awesome, also]

1. Given the definition of length above, what would the type signa- ture be? How many arguments, of what type does it take? What is the type of the result it evaluates to?

> length' :: [a] -> Integer
> length' = fromIntegral . length

2. What are the results of the following expressions?
a) length [1, 2, 3, 4, 5]
5

b) length [(1, 2), (2, 3), (3, 4)]
3

c) length allAwesome
2

d) length (concat allAwesome)
5

3. Given what we know about numeric types and the type signature of length, look at these two expressions. One works and one returns an error. Determine which will return an error and why.
(n.b., you will find Foldable t => t a representing [a], as with concat in the previous chapter. Again, consider Foldable t to represent a list here, even though list is only one of the possible types.)
     Prelude> 6 / 3
    -- and
     Prelude> 6 / length [1, 2, 3]

The former will evaluate fine, because the output of (/) is a Fractional term level value.

The latter will break because that there's no instance of Fractional for Int (the output of length).

mp> 6 / length [1, 2, 3]

<interactive>:170:1: error:
    • No instance for (Fractional Int) arising from a use of ‘/’
    • In the expression: 6 / length [1, 2, 3]
      In an equation for ‘it’: it = 6 / length [1, 2, 3]
mp>

4. How can you fix the broken code from the preceding exercise using a different division function/operator?

Use div!
mp> :t (/)
(/) :: Fractional a => a -> a -> a
mp> :t div
div :: Integral a => a -> a -> a
mp> 6 `div` length [1, 2, 3]
2
mp>

5. What is the type of the expression 2 + 3 == 5? What would we expect as a result?

2 + 3 == 5 :: Bool
True

6. What is the type and expected result value of the following:
     Prelude> x = 5
     Prelude> x + 3 == 5

x + 3 == 5 :: Bool
False

7. Below are some bits of code. Which will work? Why or why not?
If they will work, what value would these reduce to?
     Prelude> length allAwesome == 2

will work because the types line up.
Reduces to: True

     Prelude> length [1, 'a', 3, 'b']

will not work because all values in a list must be of the same type.

     Prelude> length allAwesome + length awesome

will work because the types line up.
Reduces to: 5

     Prelude> (8 == 8) && ('b' < 'a')

will work because the types line up.
Reduces to: False

     Prelude> (8 == 8) && 9

will not work because numbers aren't Bools.

8. Write a function that tells you whether or not a given String (or list) is a palindrome. Here you’ll want to use a function called reverse a predefined function that does what it sounds like.
     reverse :: [a] -> [a]
     reverse "blah"
     "halb"

> isPalindrome :: (Eq a) => [a] -> Bool
> isPalindrome x = reverse x == x

mp> isPalindrome "abcba"
True
mp> isPalindrome "abcbas"
False

9. Write a function to return the absolute value of a number using if-then-else

> myAbs :: Integer -> Integer
> myAbs x = if x >= 0
>             then x
>             else negate x

mp> myAbs 1
1
mp> myAbs (-2)
2

10. Fill in the definition of the following function, using fst and snd:

> f :: (a, b) -> (c, d) -> ((b, d), (a, c))
> f x y = ((snd x, snd y), (fst x, fst y))


Correcting syntax
In the following examples, you’ll be shown syntactically incorrect code. Type it in and try to correct it in your text editor, validating it with GHC or GHCi.

1. Here, we want a function that adds 1 to the length of a string argument and returns that result.

> x = (+)
> lenPlus1 xs = w `x` 1 -- F xs = w 'x' 1
>      where w = length xs

2. This is supposed to be the identity function, id.

> myID x = x-- \X = x

3. When fixed, this function will return 1 from the value (1, 2).
f (a b) = A

> fst' (a, b) = a -- A

Match the function names to their types
1. Which of the following types is the type of show?
a) show a => a -> String
b) Show a -> a -> String
c) Show a => a -> String

c) is correct. Show a is a type class constraint, not an argument, and it needs to be capitalized.

2. Which of the following types is the type of (==)?
a) a -> a -> Bool
b) Eq a => a -> a -> Bool
c) Eq a -> a -> a -> Bool
d) Eq a => A -> Bool

b) is correct.

3. Which of the following types is the type of fst?
a) (a, b) -> a
b) b -> a
c) (a, b) -> b

a) is the type of fst.

4. Which of the following types is the type of (+)?
a) (+) :: Num a -> a -> a -> Bool
b) (+) :: Num a => a -> a -> Bool
c) (+) :: num a => a -> a -> a
d) (+) :: Num a => a -> a -> a
e) (+) :: a -> a -> a

d) is the type of (+).
