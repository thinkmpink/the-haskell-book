> module Strings where

Exercises: Scope
1. These lines of code are from a REPL session. Is 𝑦 in scope for 𝑧?
     Prelude> x = 5
     Prelude> y = 7
     Prelude> z = x * y
Yes, y is in scope for z.

2. These lines of code are from a REPL session. Is h in scope for 𝑔? Go with your gut here.
     Prelude> f = 3
     Prelude> g = 6 * f + h
No, h is not in scope for g.

3. This code sample is from a source file. Is everything we need to execute area in scope?

> area d = pi * (r * r)
>
> r = d / 2

No, because

> d = undefined

for r.

4. This code is also from a source file. Now are 𝑟 and 𝑑 in scope for area?

> area' d = pi * (r * r)
>   where r = d / 2

Yes.


Exercises: Syntax Errors

mp> ++ [1,2,3] [4,5,6] -- will not compile

<interactive>:27:1: error: parse error on input ‘++’
mp> (++) [1,2,3] [4,5,6] -- will compile
[1,2,3,4,5,6]
mp> '<3' ++ ' Haskell' -- will not compile

<interactive>:29:2: error: parse error on input ‘<’
mp> "<3" ++ " Haskell" -- should be fine
"<3 Haskell"
mp> concat ["<3", " Haskell"] -- should be fine
"<3 Haskell"
mp>

Chapter Exercises

1. For the following lines of code, read the syntax carefully and decide if they are written correctly. Test them in your REPL after you’ve decided to check your work. Correct as many as you can.

a) concat [[1, 2, 3], [4, 5, 6]]
b) ++ [1, 2, 3] [4, 5, 6]
c) (++) "hello" " world"
d) ["hello" ++ " world]
e) 4 !! "hello"
f) (!!) "hello" 4 g) take "4 lovely"
h) take 3 "awesome"


mp> concat [[1,2,3], [4,5,6]] -- works
[1,2,3,4,5,6]
mp> ++ [1,2,3] [4,5,6] -- doesn't work

<interactive>:43:1: error: parse error on input ‘++’
mp> (++) [1,2,3] [4,5,6] -- works
[1,2,3,4,5,6]
mp> (++) "hello" " world" -- works
"hello world"
mp> ["hello" ++ " world] -- doesn't work

<interactive>:47:37: error:
    lexical error in string/character literal at end of input
mp> ["hello" ++ " world"] --  works
["hello world"]
mp> 4 !! "hello" -- doesn't work

<interactive>:49:6: error:
    • Couldn't match expected type ‘Int’ with actual type ‘[Char]’
    • In the second argument of ‘(!!)’, namely ‘"hello"’
      In the expression: 4 !! "hello"
      In an equation for ‘it’: it = 4 !! "hello"
mp> "hello" !! 4 -- works
'o'
mp> (!!) "hello" 4 -- works
'o'
mp> take "4 lovely" -- doesn't work

<interactive>:52:6: error:
    • Couldn't match expected type ‘Int’ with actual type ‘[Char]’
    • In the first argument of ‘take’, namely ‘"4 lovely"’
      In the expression: take "4 lovely"
      In an equation for ‘it’: it = take "4 lovely"
mp> take 4 "lovely" -- works
"love"
mp> take 3 "awesome" -- works
"awe"
mp>

2. Next we have two sets: the first set is lines of code and the other is a set of results. Read the code and figure out which results came from which lines of code. Be sure to test them in the REPL.

a) concat [[1 * 6], [2 * 6], [3 * 6]]
b) "rain" ++ drop 2 "elbow"
c) 10 * head [1, 2, 3]
d) (take 3 "Julie") ++ (tail "yes")
e) concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]]
Can you match each of the previous expressions to one of these results presented in a scrambled order?
a) "Jules"
b) [2,3,5,6,8,9]
c) "rainbow"
d) [6,12,18]
e) 10

a) -> d)
b) -> c)
c) -> e)
d) -> a)
e) -> b)



Building functions.


1. Given the list-manipulation functions mentioned in this chapter, write functions that take the following inputs and return the expected outputs. Do them directly in your REPL and use the take and drop functions you’ve already seen.

a) -- Given
"Curry is awesome"
-- Return
"Curry is awesome!"

mp> exclain = (++ "!")
mp> exclaim = exclain
mp> exclaim "Curry is awesome"
"Curry is awesome!"

b) -- Given
"Curry is awesome!"
-- Return
"y"

mp> getFifth s = s !! 4 : []
mp> getFifth "Curry is awesome!"
"y"


c) -- Given
"Curry is awesome!"
-- Return "awesome!"

mp> dropCurryIs = drop 9
mp> dropCurryIs "Curry is awesome!"
"awesome!"


2. Now take each of the above and rewrite it in a source file as a general function that could take different string inputs as arguments but retain the same behavior. Use a variable as the argument to your (named) functions. If you’re unsure how to do this, refresh your memory by looking at the waxOff exercise from the previous chapter and the TopOrLocal module from this chapter.


> exclaim :: String -> String
> exclaim x = x ++ "!"
>
> getFifth :: String -> String
> getFifth x = [x !! 4]
>
> dropCurryIs :: String -> String
> dropCurryIs s = drop 9 s

3. Write a function of type String -> Char which returns the third character in a String. Remember to give the function a name and apply it to a variable, not a specific String, so that it could be reused for different String inputs, as demonstrated (feel free to name the function something else. Be sure to fill in the type signature and fill in the function definition after the equals sign):

> thirdLetter :: String -> Char
> thirdLetter x = x !! 2

4. Now change that function so the string operated on is always the same and the variable represents the number of the letter you want to return (you can use “Curry is awesome!” as your string input or a different string if you prefer).

> letterIndex :: Int -> Char
> letterIndex x = "Curry is awesome!" !! x

5. Using the take and drop functions we looked at above, see if you can write a function called rvrs (an abbreviation of ‘reverse’ used because there is a function called ‘reverse’ already in Prelude, so if you call your function the same name, you’ll get an error message). rvrs should take the string “Curry is awesome” and return the result “awesome is Curry.” This may not be the most lovely Haskell code you will ever write, but it is quite possible using only what we’ve learned so far. First write it as a single function in a source file. This doesn’t need to, and shouldn’t, work for reversing the words of any sentence. You’re expected only to slice and dice this particular string with take and drop.

> rvrs :: String -> String
> rvrs s = awesome ++ " " ++ is ++ " " ++ curry' ++ "."
>   where awesome = dropCurryIs s
>         is      = take 2 $ drop 6 s
>         curry'  = take 5 s

6. Let’s see if we can expand that function into a module. Why would we want to? By expanding it into a module, we can add more functions later that can interact with each other. We can also then export it to other modules if we want to and use this code in those other modules. There are different ways you could lay it out, but for the sake of convenience, we’ll show you a sample layout so that you can fill in the blanks:

Into the parentheses after print you’ll need to fill in your function name rvrs plus the argument you’re applying rvrs to, in this case “Curry is awesome.” That rvrs function plus its argument are now the argument to print. It’s important to put them inside the parentheses so that that function gets applied and evaluated first, and then that result is printed.
Of course, we have also mentioned that you can use the $ symbol to avoid using parentheses, too. Try modifying your main function to use that instead of the parentheses.

--> see Reverse.lhs
