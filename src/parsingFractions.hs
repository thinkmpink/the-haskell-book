{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecimalOrFrac :: Parser (Either Rational Integer)
parseDecimalOrFrac =
      try (Left <$> parseFraction)
  <|> Right <$> decimal

main :: IO ()
main = do

  let parseFraction' =
        parseString parseFraction mempty
      parseDorF =
        parseString parseDecimalOrFrac mempty

  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork

  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

  -- from Exercise: Try Try
  print $ parseDorF shouldWork
  let shouldWorkNow = alsoBad
  print $ parseDorF shouldWorkNow
