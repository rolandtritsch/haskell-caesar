{-|
Module      : MainTestSpec
Description : Main module to implement the tests for the Caesar lib.
Copyright   : (c) Roland Tritsch, 2017
License     : GPL-3
Maintainer  : roland@tritsch.org
Stability   : experimental
Portability : POSIX

-}
module Main where

import Test.Hspec
import Test.QuickCheck

import Caesar

-- |For property based testing: Generate chars that are in the alphabet.
genSafeChar :: Gen Char
genSafeChar = elements alphabet

-- |For property based testing: Build strings/texts that only contain chars from the alphabet.
genSafeString :: Gen String
genSafeString = listOf genSafeChar

-- |Run all tests.
main :: IO ()
main = hspec $ do
  describe "encrypt" $ do
    it "returns bcd for abc with key 1" $ do
      encrypt "abc" 1 `shouldBe` "bcd"

    it "returns bcd for abc with key -1" $ do
      encrypt "abc" (negate 1) `shouldBe` "bcd"

    it "returns roland for roland with a key that is the length of the alphabet" $ do
      encrypt "roland" (length alphabet) `shouldBe` "roland"

    it "should return the string for all strings that are dec (enc string key) key" $ do
      property $ forAll genSafeString $ \s k -> decrypt (encrypt s k) k `shouldBe` s

    it "should return the empty string, if the text contains a char that is not in the alphabet" $ do
      encrypt "Müller" 1 `shouldBe` "Unknown char in text. Cannot encrypt."

  describe "decrypt" $ do
    it "returns abc for bcd with key 1" $ do
      decrypt "bcd" 1 `shouldBe` "abc"

    it "returns roland for roland with a key that is the length of the alphabet" $ do
      decrypt "roland" (length alphabet) `shouldBe` "roland"
