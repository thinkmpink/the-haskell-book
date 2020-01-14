Chapter 24. Parser combinators

> {-# LANGUAGE OverloadedStrings #-}

> module ParserCombinators where
>
> import Text.Trifecta

Exercises: Parsing Practice

1. There’s a combinator that’ll let us mark that we expect an input stream to be finished at a particular point in our parser. In the parsers library this is simply called eof (end-of-file) and is in the Text.Parser.Combinators module. See if you can make the one and oneTwo parsers fail because they didn’t exhaust the input stream!

see learnParsers.hs

2. Use string to make a Parser that parses “1”, “12”, and “123” out of the example input respectively. Try combining it with stop too. That is, a single parser should be able to parse all three of those strings. An example:
     Prelude> p123 "1"
     Success 1
     Prelude> p123 "12"
     Success 12
     Prelude> p123 "123"
     Success 123
You can be more creative than this with the parser if you want.

see p123 :: String -> Result Int
in src/learnParsers.hs

3. Try writing a Parser that does what string does, but using char.

see string' :: CharParsing m :: String -> m String
in src/learnParsers.hs

Exercise: Unit of Success

This should not be unfamiliar at this point, even if you do not understand all the details:

Prelude> parseString integer mempty "123abc"
Success 123
Prelude> parseString (integer >> eof) mempty "123abc"
Failure (interactive):1:4: error: expected: digit,
    end of input
123abc<EOF>
   ^
Prelude> parseString (integer >> eof) mempty "123"
Success ()

What we want you to try now is rewriting the final example so it returns the integer that it parsed instead of Success (). It should return the integer successfully when it receives an input with an integer followed by an EOF and fail in all other cases:

Prelude> parseString (yourFuncHere) mempty "123"
Success 123
Prelude> parseString (yourFuncHere) mempty "123abc"
Failure (interactive):1:4: error: expected: digit,
    end of input
123abc<EOF>
   ^

> parseIntegerEOF :: TokenParsing m => m Integer
> parseIntegerEOF = integer <* eof
