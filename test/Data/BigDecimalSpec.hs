module Data.BigDecimalSpec
  (main, spec)
where

import Test.Hspec
import Test.QuickCheck hiding (shrink)
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

--myit :: (HasCallStack, Example a1) => String -> a1 -> SpecWith (Arg a1)
--myit = modifyMaxSuccess (const 1000) it

two = BigDecimal 2 0
three = BigDecimal 3 0

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
    it "adds leading 0s if required" $
          toString (BigDecimal (-14) 10) `shouldBe` "-0.0000000014"
    it "can handle integer values" $
          toString ten `shouldBe` "10"
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
    it "is the same as *(-1)" $
          property $ \bd -> negate bd === (-one * bd :: BigDecimal)
    it "is its own inverse" $
      property $ \bd -> negate (negate bd) === (bd :: BigDecimal)

  describe "(/)" $ do
    it "divides two BigDecimals" $
      BigDecimal 16 1 / BigDecimal 4 1 `shouldBe` BigDecimal 4 0
    it "yields x for x/1 for any x" $
      property $ \x -> x/1 === (x :: BigDecimal)
    it "yields 1 for x/x any non-zero x" $
      property $ \x -> if x /= zero then x / x === one else 1===1
    it "throws an Arithmetic exception when dividing by 0" $
      property $ \bd -> evaluate (bd / zero) `shouldThrow` anyArithException
    it "yields y for (x*y)/x for any nonzero x" $
      property $ \x y -> y === if x == zero then y else (x*y)/x
    it "rounds up if next decimal would be > 5" $
      toBD "6" / toBD "9" `shouldBe` toBD "0.6667"
    it "rounds up if next decimal would be = 5" $
      toBD "5" / toBD "9" `shouldBe` toBD "0.5556"
    it "rounds down if next decimal would be < 5" $
      toBD "4" / toBD "9" `shouldBe` toBD "0.4444"

  describe "fromRational" $ do
    it "constructs a BigDecimal from a Ratio" $
      fromRational (1 :% 32) `shouldBe` one / BigDecimal 32 0
    it "works for any non-zero divisors" $
      property $ \x y -> if y == 0 then 1 ===1 else fromRational (x :% y) === BigDecimal x 0 / BigDecimal y 0

  describe "divide" $ do
    it "divides BigDecimals applying RoundingMode and precision" $
      divide (two, three) ROUND_HALF_UP (Just 9) `shouldBe` toBD "0.666666667"
    it "always rounds down when using ROUND_DOWN" $
      divide (two, three) ROUND_DOWN (Just 9) `shouldBe` toBD "0.666666666"
    it "always rounds up when using ROUND_UP" $
      divide (one, toBD "9") ROUND_UP (Just 3) `shouldBe` toBD "0.112"
    it "rounds down if next decimal would be <= 5 when using ROUND_HALF_DOWN" $
      divide (toBD "5", toBD "9") ROUND_HALF_DOWN (Just 4) `shouldBe` toBD "0.5555"
    it "rounds up if next decimal would be >= 5 when using ROUND_HALF_UP" $
      divide (toBD "5", toBD "9") ROUND_HALF_UP (Just 4) `shouldBe` toBD "0.5556"

    it "rounds to next even number if next decimal would be == 5 when using ROUND_HALF_EVEN" $
      divide (toBD "5", toBD "9") ROUND_HALF_EVEN (Just 4) `shouldBe` toBD "0.5556"




    it "throws an exception when PRECISE is used and a non-terminating decimal expansion is detected" $
      evaluate (divide (toBD "5", toBD "9") PRECISE Nothing) `shouldThrow` anyException
    it "gives a pecise value when using PRECISE and no max precision" $
      divide (1, toBD "32") PRECISE Nothing `shouldBe` toBD "0.03125"
    it "gives a pecise value when using PRECISE and a sufficient precision" $
      divide (1, toBD "32") PRECISE (Just 5) `shouldBe` toBD "0.03125"
    it "gives a pecise value when using PRECISE and a to small precision" $
      evaluate (divide (1, toBD "32") PRECISE (Just 4)) `shouldThrow` anyException



  describe "shrink" $ do
    it "removes trailing zeros while taking care of the scale" $
      shrink 0 (BigDecimal 1000 3) `shouldBe` BigDecimal 1 0
    it "does not eliminate more 0s than requested" $
      shrink 2 (BigDecimal 1000 3) `shouldBe` BigDecimal 100 2
    it "does not eliminate more 0s than possible" $
      shrink 0 (BigDecimal 1230 3) `shouldBe` BigDecimal 123 2
