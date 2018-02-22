module Data.BigDecimalSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.BigDecimal

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

instance Arbitrary BigDecimal where
    arbitrary = do
      Fixed x <- arbitrary
      NonNegative y <- arbitrary
      return $ BigDecimal x y




spec :: Spec
spec =
  describe "Data.BigDecimal.toBD" $ do
    it "reads BigDecimals from strings" $
      toBD "145.123" `shouldBe` BigDecimal 145123 3
    it "is the inverse of toString" $ property $
      \bd -> (toBD . toString) bd === (bd :: BigDecimal)
      --   $ BigDecimal i 0)
      --toBD "145.123" `shouldBe` BigDecimal 145 0
--    it "is idempotent" $ property $
--      \str -> strip str === strip (strip str)
