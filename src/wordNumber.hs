module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "unknown"

digits :: Int -> [Int]
digits n = reverse $ go $ abs n
  where go m
         | m < 10    = [m]
         | otherwise = (mod m 10):(go (div m 10))

wordNumber :: Int -> String
wordNumber n = concat
             . intersperse "-"
             . map digitToWord
             . digits $ n
