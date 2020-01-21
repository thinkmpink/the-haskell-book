-- Chapter 24. Parser combinators

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module ParserCombinators where

import Control.Applicative
import Data.Char (digitToInt)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Ratio
import Data.Time
import Text.RawString.QQ
import Text.Read (readMaybe)
import Text.Trifecta

-- Exercises: Parsing Practice

-- 1. There’s a combinator that’ll let us mark that we expect an input stream to be finished at a particular point in our parser. In the parsers library this is simply called eof (end-of-file) and is in the Text.Parser.Combinators module. See if you can make the one and oneTwo parsers fail because they didn’t exhaust the input stream!

-- see learnParsers.hs

-- 2. Use string to make a Parser that parses “1”, “12”, and “123” out of the example input respectively. Try combining it with stop too. That is, a single parser should be able to parse all three of those strings. An example:
--      Prelude> p123 "1"
--      Success 1
--      Prelude> p123 "12"
--      Success 12
--      Prelude> p123 "123"
--      Success 123
-- You can be more creative than this with the parser if you want.

-- see p123 :: String -> Result Int
-- in src/learnParsers.hs

-- 3. Try writing a Parser that does what string does, but using char.

-- see string' :: CharParsing m :: String -> m String
-- in src/learnParsers.hs

-- Exercise: Unit of Success

-- This should not be unfamiliar at this point, even if you do not understand all the details:

-- Prelude> parseString integer mempty "123abc"
-- Success 123
-- Prelude> parseString (integer >> eof) mempty "123abc"
-- Failure (interactive):1:4: error: expected: digit,
--     end of input
-- 123abc<EOF>
--    ^
-- Prelude> parseString (integer >> eof) mempty "123"
-- Success ()

-- What we want you to try now is rewriting the final example so it returns the integer that it parsed instead of Success (). It should return the integer successfully when it receives an input with an integer followed by an EOF and fail in all other cases:

-- Prelude> parseString (yourFuncHere) mempty "123"
-- Success 123
-- Prelude> parseString (yourFuncHere) mempty "123abc"
-- Failure (interactive):1:4: error: expected: digit,
--     end of input
-- 123abc<EOF>
--    ^

parseIntegerEOF :: TokenParsing m => m Integer
parseIntegerEOF = integer <* eof

-- Exercise: Try Try
-- Make a parser, using the existing fraction parser plus a new decimal parser, that can parse either decimals or fractions. You’ll want to use <|> from Alternative to combine the...alternative parsers. If you find this too difficult, write a parser that parses straightforward integers or fractions. Make a datatype that contains either an integer or a rational and use that datatype as the result of the parser. Or use Either. Run free, grasshopper.
-- Hint: we’ve not explained it yet, but you may want to try try.

-- see parsingFractions.hs


-- Lexers and Parsers

p' :: Parser [Integer]
p' = some $ do
  i <- token (some digit)
  return (read i)


-- Chapter Exercises

-- 1. Write a parser for semantic versions as defined by http://semver.org/. After making a working parser, write an Ord instance for the SemVer type that obeys the specification outlined on the SemVer website.

data NumberOrString =
         NOSS String
       | NOSI Integer
      deriving (Eq, Show)

instance Ord NumberOrString where
  compare (NOSS a) (NOSS b) = compare a b
  compare (NOSI i) (NOSI j) = compare i j
  compare (NOSS _) (NOSI _) = GT
  compare (NOSI _) (NOSS _) = LT

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Show, Eq)

instance Ord SemVer where
  compare (SemVer maj1 min1 pat1 rel1 _)
          (SemVer maj2 min2 pat2 rel2 _)
    | maj1 < maj2                    = LT
    | maj1 > maj2                    = GT
    | min1 < min2                    = LT
    | min1 > min2                    = GT
    | pat1 < pat2                    = LT
    | pat1 > pat2                    = GT
    | null rel1 && null rel2         = EQ
    | null rel1 && (not $ null rel2) = GT
    | (not $ null rel1) && null rel2 = LT
    | otherwise       = compare rel1 rel2

-- Applicative version of parseSemVer
-- TODO: remove use of option
parseSemVer' :: Parser SemVer
parseSemVer' =
  SemVer <$> integer
         <* char '.'
         <*> integer
         <* char '.'
         <*> integer
         <*> option [] (char '-' *> parseRelease)
         <*> option [] (char '+' *> parseMetadata)

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  _     <- char '.'
  minor <- integer
  _     <- char '.'
  patch <- integer

  release  <- option [] (char '-' *> parseRelease)
  metadata <- option [] (char '+' *> parseMetadata)
  return (SemVer major minor patch release metadata)

parseRelease :: Parser Release
parseRelease = sepBy parseNumOrString (char '.')

parseMetadata :: Parser Metadata
parseMetadata = sepBy parseNumOrString (char '.')

parseNumOrString :: Parser NumberOrString
parseNumOrString = do
  chars <- some $ token alphaNum
  return $ case readMaybe chars of
    Nothing -> NOSS chars
    Just n  -> NOSI n

-- 2. Write a parser for positive integer values. Don’t reuse the pre-existing digit or integer functions, but you can use the rest of the libraries we’ve shown you so far. You are not expected to write a parsing library from scratch.

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']
         <?> "One of ['0',...,'9']"

base10Integer :: Parser Integer
base10Integer = (snd . toInt)
            <$> some parseDigit
  where
    toInt =
      foldr
        (\d (tens, n) ->
          let new   = fromIntegral $ digitToInt d
              n'    = new * tens + n
              tens' = tens * 10
          in (tens', n'))
        (1, 0)

-- 3. Extend the parser you wrote to handle negative and positive integers. Try writing a new parser in terms of the one you already have to do this.

base10Integer' :: Parser Integer
base10Integer' =
      char '-' *> pure negate <*> base10Integer
  <|> base10Integer

-- 4. Write a parser for US/Canada phone numbers with varying formats.

-- aka area code
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone =
      try parseHyphenFmtNoCountry
  <|> try parseNoPuncFmt
  <|> try parseHyphenFmtCountry
  <|> parseParenFmt
  where
    parseHyphenFmtNoCountry =
      PhoneNumber <$> parseNumPlanArea
                  <* char '-'
                  <*> parseExchange
                  <* char '-'
                  <*> parseLineNum

    parseNoPuncFmt =
      PhoneNumber <$> parseNumPlanArea
                  <*> parseExchange
                  <*> parseLineNum

    parseParenFmt =
      PhoneNumber <$ char '('
                  <*> parseNumPlanArea
                  <* char ')' <* char ' '
                  <*> parseExchange
                  <* char '-'
                  <*> parseLineNum

    parseHyphenFmtCountry =
      PhoneNumber <$  string "1-"
                  <*> parseNumPlanArea
                  <*  char '-'
                  <*> parseExchange
                  <* char '-'
                  <*> parseLineNum

    parseNumPlanArea = read <$> count 3 digit
    parseExchange = read <$> count 3 digit
    parseLineNum = read <$> count 4 digit

-- 5. Write a parser for a log file format and sum the time spent in each activity. Additionally, provide an alternative aggregation of the data that provides average time spent per activity per day. The format supports the use of comments which your parser will have to ignore. The # characters followed by a date mark the beginning of a particular day.
-- Log format example:

logEx :: String
logEx = logEx1 <> logEx2

logEx1 :: String
logEx1 = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
|]

logEx2 :: String
logEx2 = [r|
# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

-- You are to derive a reasonable datatype for representing this data yourself. For bonus points, make this bi-directional by making a Show representation for the datatype which matches the format you are parsing. Then write a generator for this data using QuickCheck’s Gen and see if you can break your parser with QuickCheck.

newtype ActivityName =
  ActivityName String
  deriving (Eq, Ord, Show)

data Activity =
  Activity  {
    activityTime :: LocalTime
  , activityName :: ActivityName
  } deriving Show

instance Eq Activity where
  Activity _ n1 == Activity _ n2 =
    n1 == n2

-- assumes input sorted by time
totalTime :: [Activity]
          -> M.Map ActivityName NominalDiffTime
totalTime (Activity t1 n1 : Activity t2 n2 : as) =
  M.insertWith (+) n1 (diffLocalTime t2 t1) $
    totalTime (Activity t2 n2 : as)
totalTime (Activity t n : []) =
  M.insertWith (+) n 0 $ totalTime []
totalTime [] = M.empty

averageTime :: [Activity]
            -> M.Map ActivityName NominalDiffTime
averageTime as =
  let totals   = totalTime as
      divisors = M.fromList $
        (,) <$> activityName . head
            <*> length
            <$> (L.group $ L.sortOn activityName as)

  in M.intersectionWith divBy totals divisors

divBy :: NominalDiffTime
      -> Int
      -> NominalDiffTime
divBy ndt i =
    fromRational
  . flip (/) (fromIntegral i)
  . toRational $ ndt

parseLocalTime :: Day -> Parser LocalTime
parseLocalTime day =
  LocalTime day <$> parseTimeOfDay

parseTimeOfDay :: Parser TimeOfDay
parseTimeOfDay =
      makeTimeOfDayValid
  <$> (read <$> many digit)
  <*  char ':'
  <*> (read <$> many digit)
  <*> pure 0 -- the format lacks picosecond info
  >>= handleMayTime

  where
    handleMayTime Nothing  = fail "Invalid time"
    handleMayTime (Just t) = pure t

parseDay :: Parser Day
parseDay =
      fromGregorianValid
  <$  string "# "
  <*> integer
  <*  char '-'
  <*> (read <$> many digit)
  <*  char '-'
  <*> (read <$> many digit)
  >>= handleMayDay

  where
    handleMayDay Nothing  = fail "Invalid day"
    handleMayDay (Just d) = pure d

parseActivityName :: Parser ActivityName
parseActivityName =
  ActivityName <$> (concat <$> many validStr)
  where
    notEndCand = some (noneOf "\n- \t")
    notComment = string "-" <* notFollowedBy (char '-')
    notSpBeforeComment =
      some (oneOf " \t") <* notFollowedBy (string "--")
    validStr   = try notComment
             <|> try notSpBeforeComment
             <|> notEndCand


parseActivity :: Day -> Parser Activity
parseActivity day =
  Activity <$> parseLocalTime day
           <*  char ' '
           <*> parseActivityName

parseCommentOrEOL :: Parser String
parseCommentOrEOL =
  string "\n" <|> parseComment

parseComment :: Parser String
parseComment =
     many (oneOf " \t")
  *> string "-- "
  *> some (noneOf "\n")
  <* char '\n'

parseDayActivities :: Parser [Activity]
parseDayActivities = do
  day <- parseDay
  _   <- parseCommentOrEOL
  endBy (parseActivity day) parseCommentOrEOL

parseLog :: Parser [Activity]
parseLog =
  fmap concat $ many $
    surroundedBy parseDayActivities $
      many parseCommentOrEOL

-- TODO: impl generation, QuickCheck
