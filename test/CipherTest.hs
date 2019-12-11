module CipherTest where

import Cipher
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "caesar" $ do
    it "is inverted by unCaesar" $ do
      property prop_unCaesarInvCaesar
  describe "vigenere" $ do
    it "is inverted by unVigenere" $ do
      property prop_unVigenereInvVigenere

genCipherBounds :: Gen CipherBounds
genCipherBounds = do
  a <- arbitrary :: Gen Char
  b <- arbitrary :: Gen Char
  return $ if a < b
           then CipherBounds a b
           else CipherBounds b a

genPlaintext :: CipherBounds -> Gen String
genPlaintext (CipherBounds low up) =
  listOf1 . elements $ enumFromTo low up

genCaesar :: Gen (CipherBounds, String, Int)
genCaesar = do
  cb    <- genCipherBounds
  pt    <- genPlaintext cb
  shift <- arbitrary :: Gen Int
  return (cb, pt, shift)

genKeyword :: Gen Keyword
genKeyword = fmap Keyword $ listOf1 (arbitrary :: Gen Char)

genVigenere :: Gen (CipherBounds, Keyword, String)
genVigenere = do
  cb    <- genCipherBounds
  kw    <- genKeyword
  pt    <- genPlaintext cb
  return (cb, kw, pt)

prop_unCaesarInvCaesar :: Property
prop_unCaesarInvCaesar = forAll genCaesar $
  \(cb, pt, shift) ->
    let scrambled   = caesar cb shift pt
        unscrambled = unCaesar cb shift scrambled
    in unscrambled == pt

prop_unVigenereInvVigenere :: Property
prop_unVigenereInvVigenere = forAll genVigenere $
  \(cb, kw, pt) ->
    let ct  = vigenere cb kw pt
        pt2 = unVigenere cb kw ct
    in pt == pt2
