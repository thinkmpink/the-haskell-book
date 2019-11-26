
> module Recursion where
>
> applyTimes :: (Eq a, Num a) =>
>                a -> (b -> b) -> b -> b
> applyTimes 0 f b = b
> applyTimes n f b = f (applyTimes (n-1) f b)

Intermission: Exercise
Write out the evaluation of the following. It might be a little less noisy if you do so with the form that didn't use (.).

applyTimes 5 (+1) 5
= (+1) (applyTimes 4 (+1) 5)
= (+1) ((+1) (applyTimes 3 (+1) 5))
= (+1) ((+1) ((+1) (applyTimes 2 (+1) 5)))
= (+1) ((+1) ((+1) ((+1) (applyTimes 1 (+1) 5))))
= (+1) ((+1) ((+1) ((+1) ((+1) (applyTimes 0 (+1) 5)))))
= (+1) ((+1) ((+1) ((+1) ((+1) 5))))
= (+1) ((+1) ((+1) ((+1) 6)))
= (+1) ((+1) ((+1) 7))
= (+1) ((+1) 8)
= (+1) 9
= 10


8.6 Chapter Exercises
1. What is the type of [[True, False], [True, True], [False, True]] ?
a) Bool
b) mostly True
c) [a]
d) [[Bool]]

d) [[Bool]] is correct.

2. Which of the following has the same type as [[True, False], [True, True], [False, True]]?
a) [(True, False), (True, True), (False, True)]
b) [[3 == 3], [6 > 5], [3 < 4]]
c) [3 == 3, 6 > 5, 3 < 4]
d) ["Bool", "more Bool", "Booly Bool!"]

b) is correct.

3. For the following function
func :: [a] -> [a] -> [a]
func x y = x ++ y
which of the following is true?
a) x and y must be of the same type
b) x and y must both be lists
c) if x is a String then y must be a String
d) all of the above

d) is correct.

4. For the func code above, which is a valid application of func to both of its arguments?
a) func "Hello World"
b) func "Hello" "World"
c) func [1, 2, 3] "a, b, c"
d) func ["Hello", "World"]

b) is correct.


Reviewing currying
Given the following definitions, tell us what value results from further applications.

> cattyConny :: String -> String -> String
> cattyConny x y = x ++ " mrow " ++ y

> -- fill in the types
> flippy :: String -> String -> String
> flippy = flip cattyConny

> appedCatty :: String -> String
> appedCatty = cattyConny "woops"

> frappe :: String -> String
> frappe = flippy "haha"

1. What is the value of appedCatty "woohoo!" ? Try to determine the answer for yourself, then test in the REPL.

appedCatty "woohoo!" :: String
appedCatty "woohoo!" = "woops mrow woohoo!"

2. frappe "1"

frappe "1" :: String
frappe "1" = "1 mrow haha"

3. frappe (appedCatty "2")

frappe (appedCatty "2") :: String
frappe (appedCatty "2") = "woops mrow 2 mrow haha"

4. appedCatty (frappe "blue")
appedCatty (frappe "blue") :: String
appedCatty (frappe "blue") = "woops mrow blue mrow haha"

5. cattyConny (frappe "pink")
              (cattyConny "green" (appedCatty "blue"))
 = cattyConny "pink mrow haha"
              (cattyConny "green" "woops mrow blue")
 = cattyConny "pink mrow haha"
              "green mrow woops mrow blue"
 = "pink mrow haha mrow green mrow woops mrow blue"

6. cattyConny (flippy "Pugs" "are") "awesome"
 = cattyConny "are mrow Pugs" "awesome"
 = "are mrow Pugs mrow awesome"


Recursion

> dividedBy :: Integral a => a -> a -> (a, a)
> dividedBy num denom = go num denom 0
>   where go n d count
>          | n < d = (count, n)
>          | otherwise = go (n - d) d (count + 1)

1. Write out the steps for reducing dividedBy 15 2 to its final answer according to the Haskell code.

dividedBy 15 2
 = go 15 2 0
 = go 13 2 1
 = go 11 2 2
 = go 9  2 3
 = go 7  2 4
 = go 5  2 5
 = go 3  2 6
 = go 1  2 7
 = (7, 1)


 2. Write a function that recursively sums all numbers from 1 to n, n being the argument. So that if n was 5, you’d add 1 + 2 + 3 + 4 + 5 to get 15. The type should be (Eq a, Num a) => a -> a.

> sumR :: (Eq a, Num a) => a -> a
> sumR 0 = 0
> sumR n = n + sumR (n-1)

3. Write a function that multiplies two integral numbers using recursive summation. The type should be (Integral a) => a -> a -> a.

> multR :: (Integral a) => a -> a -> a
> multR a b
>   | a == 1    = b
>   | otherwise = b + multR (a-1) b
