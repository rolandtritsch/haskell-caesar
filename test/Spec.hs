import Test.Hspec
import Test.QuickCheck

import Caesar

genSafeChar :: Gen Char
genSafeChar = elements alphabet

genSafeString :: Gen String
genSafeString = listOf genSafeChar

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

  describe "decrypt" $ do
    it "returns abc for bcd with key 1" $ do
      decrypt "bcd" 1 `shouldBe` "abc"

    it "returns roland for roland with a key that is the length of the alphabet" $ do
      decrypt "roland" (length alphabet) `shouldBe` "roland"
