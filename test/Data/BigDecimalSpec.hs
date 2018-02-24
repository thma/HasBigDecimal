module Data.BigDecimalSpec
--  (main, spec)
where

import Test.Hspec
import Test.QuickCheck hiding (shrink)
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
  describe "toBD" $ do
    it "reads BigDecimals from strings" $
      toBD "-145.123" `shouldBe` BigDecimal (-145123) 3
    it "is inverse of toString" $
      property $ \bd -> (toBD . toString) bd === (bd :: BigDecimal)

  describe "toString" $ do
    it "converts BigDecimals to string" $
      toString (BigDecimal (-145123) 3) `shouldBe` "-145.123"
    it "is inverse of toBD" $
      property $ \bd -> (toString . toBD . toString) bd === toString (bd :: BigDecimal)

  describe "read" $ do
    it "reads BigDecimals from strings in constructor notation" $
      read "BigDecimal 76878 5" `shouldBe` BigDecimal 76878 5
    it "is inverse of show" $
      property $ \bd -> (read . show) bd === (bd :: BigDecimal)

  describe "show" $ do
    it "converts BigDecimals to strings in constructor notation" $
      show (BigDecimal 76878 5) `shouldBe` "BigDecimal 76878 5"
    it "is inverse of read" $
      property $ \bd -> (read . show) bd === (bd :: BigDecimal)

  describe "(+)" $ do
    it "adds two BigDecimals" $
      BigDecimal 73 1 + BigDecimal 270 2 `shouldBe` BigDecimal 1000 2
    it "has 0 as neutral element" $
      property $ \bd -> bd + zero === (bd :: BigDecimal)
    it "adds x to (-x) yielding 0" $
      property $ \bd -> shrink 0 (bd + (-bd)) === zero
    it "uses the max scale of the summands" $
      property $ \ai as bi bs -> max as bs === getScale (BigDecimal ai as + BigDecimal bi bs)
    it "uses Integer addition when summands have same scale" $
      property $ \ai bi scale -> ai + bi === getValue (BigDecimal ai scale + BigDecimal bi scale)
    it "matches values when scaling" $
      property $ \ai bi scale -> getValue (BigDecimal ai scale + BigDecimal bi (scale+1)) === 10*ai + bi