module WordNumberTest where

import Test.Hspec
import Test.QuickCheck
import WordNumber
  (digitToWord, digits, wordNumber)

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]
    it "precision of input equals\
        \ length of output" $ do
      property prop_precisionIsLength

  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100
        `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001
        `shouldBe` "nine-zero-zero-one"
    it "hyphen count is \
      \one less than precision" $ do
      property prop_hyphenOneLessThanPrecision

prop_precisionIsLength :: Int -> Bool
prop_precisionIsLength x =
  precision x == length (digits x)

prop_hyphenOneLessThanPrecision :: Int -> Bool
prop_hyphenOneLessThanPrecision i =
  hyphenCount i == precision i - 1
  where hyphenCount =
            length
          . filter (== '-')
          . wordNumber

precision :: Int -> Int
precision n
  | n < 0     = precision (abs n)
  | n < 10    = 1
  | otherwise = 1 + precision (div n 10)
