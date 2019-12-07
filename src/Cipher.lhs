Ch 9.
We’ll still be using Data.Char for this next exercise. You should save these exercises in a module called Cipher because we’ll be coming back to them in later chapters. You’ll be writing a Caesar cipher for now, but we’ll suggest some variations on the basic program in later chapters.
A Caesar cipher is a simple substitution cipher, in which each letter is replaced by the letter that is a fixed number of places down the alphabet from it. You will find variations on this all over the place
– you can shift leftward or rightward, for any number of spaces. A rightward shift of 3 means that ’A’ will become ’D’ and ’B’ will become
CHAPTER9. THISTHINGANDSOMEMORESTUFF 336
’E,’ for example. If you did a leftward shift of 5, then ’a’ would become ’v’ and so forth.
Your goal in this exercise is to write a basic Caesar cipher that shifts rightward. You can start by having the number of spaces to shift fixed, but it’s more challenging to write a cipher that allows you to vary the number of shifts so that you can encode your secret messages differently each time.
There are Caesar ciphers written in Haskell all over the internet, but to maximize the likelihood that you can write yours without peeking at those, we’ll provide a couple of tips. When yours is working the way you want it to, we would encourage you to then look around and compare your solution to others out there.

Using these functions is optional; there are other ways you can proceed with shifting, but using chr and ord might simplify the process a bit.
You want your shift to wrap back around to the beginning of the alphabet, so that if you have a rightward shift of 3 from ’z,’ you end up back at ’c’ and not somewhere in the vast Unicode hinterlands. Depending on how you’ve set things up, this might be a bit tricky. Consider starting from a base character (e.g., ’a’) and using mod to ensure you’re only shifting over the 26 standard characters of the English alphabet.
You should include an unCaesar function that will decipher your text as well. In a later chapter, we will test it.

> module Cipher where
>
> import Data.Char
>
> data CipherBounds = CipherBounds {
>   lowerBound :: Char
> , upperBound :: Char
> }

> range :: CipherBounds -> Int
> range cb = ord (upperBound cb) - ord (lowerBound cb) + 1

Should this type signature use Natural instead of Int??

> rightShift :: CipherBounds -> Int -> Char -> Char
> rightShift cb amt ch =
>   fromZero $ shift $ toZero cb ch
>     where
>       shift i    = (i+amt) `mod` (range cb)
>       fromZero z = chr $ z + ord (lowerBound cb)

> toZero :: CipherBounds -> Char -> Int
> toZero cb ch = ord ch - ord (lowerBound cb)

> caesar :: CipherBounds -> Int -> String -> String
> caesar cb amt = map (rightShift cb amt)

> newtype Keyword = Keyword String
>   deriving (Eq, Show)

> vigenere :: CipherBounds
>          -> Keyword
>          -> String
>          -> String
> vigenere cb (Keyword s) = zipWith rt (cycle s)
>   where
>     rt key pt = rightShift cb (toZero cb key) pt
>
> promptForPlaintext :: IO String
> promptForPlaintext = do
>   putStrLn "Enter some text to encrypt: "
>   getLine
>
> defaultLBounds :: CipherBounds
> defaultLBounds = CipherBounds 'a' 'z'
>
> userCaesar9 :: IO String
> userCaesar9 = promptForPlaintext >>=
>     return
>   . caesar defaultLBounds 9
>   . map toLower
>
> promptForKey :: IO String
> promptForKey = do
>   putStrLn "Enter a key to encrypt with: "
>   getLine
>
> userVigenere :: IO String
> userVigenere = do
>   k  <- promptForKey
>   pt <- promptForPlaintext
>   return $ vigenere
>     defaultLBounds
>     (Keyword k)
>     (map toLower pt)
