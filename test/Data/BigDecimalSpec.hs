module Data.BigDecimalSpec
  (main, spec)
where

import Test.Hspec
import Test.QuickCheck --hiding (shrink)
import Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import Data.BigDecimal
import GHC.Real (Ratio((:%))) -- we only need the Ratio Constructor
import Control.Exception (evaluate)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- arbitrary BigDecimals can be constructed using any Integer as unscaled value
-- and any non-negative Integer as scale
instance Arbitrary BigDecimal where
    arbitrary = do
      unscaledValue <- arbitrary
      NonNegative scale <- arbitrary
      return $ BigDecimal unscaledValue scale

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
    modifyMaxSuccess (const 1000) $ it "has 0 as neutral element" $
      property $ \bd -> bd + zero === (bd :: BigDecimal)
    it "adds x to (-x) yielding 0" $
      property $ \bd -> bd + (-bd) === zero
    it "uses the max scale of the summands" $
      property $ \ai as bi bs -> max as bs === getScale (BigDecimal ai as + BigDecimal bi bs)
    it "uses Integer addition when summands have same scale" $
      property $ \ai bi scale -> ai + bi === getValue (BigDecimal ai scale + BigDecimal bi scale)
    it "matches values when scaling" $
      property $ \ai bi scale -> getValue (BigDecimal ai scale + BigDecimal bi (scale+1)) === 10*ai + bi

  describe "(*)" $ do
    it "multiplies BigDecimals" $
      BigDecimal 12 1 * BigDecimal 12 2 `shouldBe` BigDecimal 144 3
    it "has 1 as neutral element" $
      property $ \bd -> bd * one === (bd :: BigDecimal)
    it "has 0 as zero element" $
      property $ \bd -> bd * zero === zero
    it "Uses Integer multiplication" $
      property $ \ai as bi -> BigDecimal ai as * BigDecimal bi 0 === BigDecimal (ai*bi) as
    it "adds the scales of the multiplicands" $
      property $ \ai as bi bs -> BigDecimal ai as * BigDecimal bi bs === BigDecimal (ai*bi) (as+bs)

  describe "abs" $ do
    it "determines the absolute value of a BigDecimal" $
      abs (BigDecimal (-12) 4)  `shouldBe` BigDecimal 12 4
    it "is idempotent" $
      property $ \bd -> (abs . abs) bd === (abs bd :: BigDecimal)
    it "is based on abs for Integers" $
      property $ \ai as -> abs (BigDecimal ai as) === BigDecimal (abs ai) as
    it "negates for input < 0" $
      property $ \bd -> abs bd === if getValue bd < 0 then negate bd else bd

  describe "signum" $ do
    it "determines the signature a BigDecimal" $
      signum (BigDecimal (-12) 4)  `shouldBe` -one
    it "returns one if input > 0, zero if input == 0 and -one if input < 0" $
      property $ \ai as -> signum (BigDecimal ai as) === if ai > 0 then one else if ai == 0 then zero else -one
    it "is based on signum for Integers" $
      property $ \ai as -> signum (BigDecimal ai as) === BigDecimal (signum ai) 0

  describe "fromInteger" $ do
    it "constructs a BigDecimal from an Integer" $
      fromInteger 1234  `shouldBe` BigDecimal 1234 0
    it "works for any Integer" $
      property $ \i -> fromInteger i === BigDecimal i 0

  describe "negate" $ do
    it "negates a BigDecimal" $
      negate (BigDecimal 1234 1)  `shouldBe` -BigDecimal 1234 1
    it "works for any BigDecimal" $
      property $ \bd -> negate bd === (-bd :: BigDecimal)
    it "is its own inverse" $
      property $ \bd -> negate (negate bd) === (bd :: BigDecimal)

  describe "(/)" $ do
    it "divides two BigDecimals" $
      BigDecimal 16 1 / BigDecimal 4 1 `shouldBe` BigDecimal 4 0
    it "yields x for x/1 for any x" $
      property $ \x -> x/1 === (x :: BigDecimal)
    it "yields 1 for x/x any non-zero x" $
      property $ \x -> one === if x == zero then one else x / x
    it "throws an Arithmetic exception when dividing by 0" $
      property $ \bd -> evaluate (bd / zero) `shouldThrow` anyArithException
    it "yields y for (x*y)/x for any nonzero x" $
      property $ \x y -> y === if x == zero then y else (x*y)/x

  describe "fromRational" $ do
    it "constructs a BigDecimal from a Ratio" $
      fromRational (1 :% 32) `shouldBe` one / BigDecimal 32 0
    it "works for any non-zero divisors" $
      property $ \x y -> if y == 0 then 1 ===1 else fromRational (x :% y) === BigDecimal x 0 / BigDecimal y 0
