
> module Mult1 where
>
> mult1     = x * y
>   where x = 5
>         y = 6
>
> ex1 = x * 3 + y
>   where x = 3
>         y = 1000
>
> ex2 = x * 5
>   where y = 10
>         x = 10 * 5 + y
>
> ex3 = z / x + y
>   where x = 7
>         y = negate x
>         z = y * 10

Formal chapter exercises

Parenthesization. Reformulate with explicit parentheses without changing the value
1. 2 + 2 * 3 - 1

> paren1 = 2 + 2 * 3 - 1 == 2 + (2 * 3) - 1

2. (^) 10 $ 1 + 1

> paren2 = ((^) 10 $ 1 + 1) == (^) 10 (1 + 1)

3. 2 ^ 2 * 4 ^ 5 + 1

> paren3 = 2 ^ 2 * 4 ^ 5 + 1 == (2^2) * (4^5) + 1

Equivalent expressions. Which of the following pairs of expressions will return the same result when evaluated?

1. 1 + 1
   2

> equiv1 = 1 + 1 == 2

2. 10 ^ 2
   10 + 9 * 10

10 ^ 2 == 10 * 10
       == (1 + 9) * 10
       == 1 * 10 + 9 * 10
       == 10 + 9 * 10

> equiv2 = 10 ^ 2 == 10 + 9 * 10

3. 400 - 37
   (-) 37 400

(-) 37 400 == 37 - 400
            < 0

400 - 37    > 0

> equiv3 = 400 - 37 /= (-) 37 400

4. 100 `div` 3
   100 / 3

div produces an integer rounded toward 0, whereas (/) uses floating-point precision

> equiv4 = fromIntegral (100 `div` 3) /= 100 / 3

5. 2 * 5 + 18
   2 * (5 + 18)

2 * 5 + 18 == (2 * 5) + 18
           == (10) + 18
           == 28

2 * (5 + 18) == 2 * (23)
             == 46
             /= 28

> equiv5 = 2 * 5 + 18 /= 2 * (5 + 18)

More fun with functions.

1. Now  you have a value called waxOn in your REPL what do you think will happen if you enter:

*Mult1> z = 7
*Mult1> y = z + 8
*Mult1> x = y ^ 2
*Mult1> waxOn = x * 5
*Mult1> waxOn
1125
*Mult1> 10 + waxOn
1135
*Mult1> (+10) waxOn
1135
*Mult1> (-) 15 waxOn
-1110
*Mult1> (-) waxOn 15
1110

2. Earlier we looked at a function called triple. While your REPL has waxOn in session, re-enter the triple function at the prompt:

*Mult1> triple x = x * 3


3. Now, what will happen if we enter this at our GHCi prompt? What do you think will happen first, considering what role waxOn is playing in this function call? Then enter it, see what does happen, and check your understanding:
triple waxOn

*Mult1> triple waxOn
3375


4. Rewrite waxOn as an expression with a where clause in your source file. Load it into your REPL and make sure it still works as expected.

> waxOn = x * 5
>   where z = 7
>         x = y ^ 2
>         y = z + 8

5. To the same source file where you have waxOn, add the triple function. Remember: the function name should be at the left margin (that is, not nested as one of the waxOn expressions). Make sure it works by loading it into your REPL and then entering triple waxOn again at the REPL prompt. You should have the same answer as you did above.

> triple x = x * 3

*Mult1> triple waxOn
3375

6. Now, without changing what you’ve done so far in that file, add a new function called waxOff that looks like this:
waxOff x = triple x

> waxOff x = triple x

7. Load the source file into your REPL and enter waxOff waxOn at
the prompt.
You now have a function, waxOff that can be applied to a variety of arguments – not just waxOn but any (numeric) value you want to substitute for 𝑥. Play with that a bit. What is the result of waxOff 10 or waxOff (-50)? Try modifying your waxOff function to do something new – perhaps you want to first triple the 𝑥 value and then square it or divide it by 10. Spend some time getting comfortable with modifying the source file code, reloading it, and checking your modification in the REPL.

> waxOff' = (^2) . triple

> waxOff'' = const 3 . div . triple

> waxOff''' = const 3 . (/) . triple
