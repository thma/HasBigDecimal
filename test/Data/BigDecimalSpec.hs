module Data.BigDecimalSpec
--  (main, spec)
where

import Test.Hspec
import Test.QuickCheck
import Data.BigDecimal

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

instance Arbitrary BigDecimal where
    arbitrary = do
      x <- arbitrary
      NonNegative y <- arbitrary
      return $ BigDecimal x y


spec :: Spec
spec = do
  describe "Data.BigDecimal.toBD" $ do
    it "reads BigDecimals from strings" $
      toBD "-145.123" `shouldBe` BigDecimal (-145123) 3
    it "is inverse of toString" $
      property $ \bd -> (toBD . toString) bd === (bd :: BigDecimal)

  describe "Data.BigDecimal.toString" $ do
    it "converts BigDecimals to string" $
      toString (BigDecimal (-145123) 3) `shouldBe` "-145.123"
    it "is inverse of toBD" $
      property $ \bd -> (toString . toBD . toString) bd === toString (bd :: BigDecimal)

  describe "Data.BigDecimal.read" $ do
    it "reads BigDecimals from strings in constructor notation" $
      read "BigDecimal 76878 5" `shouldBe` BigDecimal 76878 5
    it "is inverse of show" $
      property $ \bd -> (read . show) bd === (bd :: BigDecimal)

  describe "Data.BigDecimal.show" $ do
    it "converts BigDecimals to strings in constructor notation" $
      show (BigDecimal 76878 5) `shouldBe` "BigDecimal 76878 5"
    it "is inverse of read" $
      property $ \bd -> (read . show) bd === (bd :: BigDecimal)

  describe "Data.BigDecimal.(+)" $ do
    it "adds two BigDecimals" $
      BigDecimal 73 1 + BigDecimal 27 1 `shouldBe` BigDecimal 100 1
    it "has 0 as neutral element" $
      property $ \bd -> bd + zero === (bd :: BigDecimal)