module Data.BigDecimalSpec
  (main, spec)
where

import           Control.Exception     (evaluate)
import           Data.BigDecimal
import           GHC.Real              (Ratio ((:%)))
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import           Test.QuickCheck       hiding (shrink)

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

spec :: Spec
spec = do
  describe "toBD" $ do
    it "reads BigDecimals from strings" $
      fromString "-145.123" `shouldBe` BigDecimal (-145123) 3
    it "is inverse of toString" $
      property $ \bd -> (fromString . toString) bd === (bd :: BigDecimal)

  describe "toString" $ do
    it "converts BigDecimals to string" $
      toString (BigDecimal (-145123) 3) `shouldBe` "-145.123"
    it "adds leading 0s if required" $
      toString (BigDecimal (-14) 10) `shouldBe` "-0.0000000014"
    it "can handle integer values" $
      toString 10 `shouldBe` "10"
    it "is inverse of toBD" $
      property $ \bd -> (toString . fromString . toString) bd === toString (bd :: BigDecimal)

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
      property $ \bd -> bd + 0 === (bd :: BigDecimal)
    it "adds x to (-x) yielding 0" $
      property $ \bd -> bd + (-bd) === (0 :: BigDecimal)
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
      property $ \bd -> bd * 1 === (bd :: BigDecimal)
    it "has 0 as zero element" $
      property $ \bd -> bd * 0 === (0 :: BigDecimal)
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
      signum (BigDecimal (-12) 4)  `shouldBe` -1
    it "returns 1 if input > 0, zero if input == 0 and -1 if input < 0" $
      property $ \ai as -> signum (BigDecimal ai as) === if ai > 0 then 1 else if ai == 0 then 0 else -1
    it "is based on signum for Integers" $
      property $ \ai as -> signum (BigDecimal ai as) === BigDecimal (signum ai) 0

  describe "fromInteger" $ do
    it "constructs a BigDecimal from an Integer" $
      1234  `shouldBe` BigDecimal 1234 0
    it "works for any Integer" $
      property $ \i -> fromInteger i === BigDecimal i 0

  describe "negate" $ do
    it "negates a BigDecimal" $
      negate (BigDecimal 1234 1)  `shouldBe` -BigDecimal 1234 1
    it "works for any BigDecimal" $
      property $ \bd -> negate bd === (-bd :: BigDecimal)
    it "is the same as *(-1)" $
      property $ \bd -> negate bd === (-1 * bd :: BigDecimal)
    it "is its own inverse" $
      property $ \bd -> negate (negate bd) === (bd :: BigDecimal)

  describe "(/)" $ do
    it "divides two BigDecimals" $
      BigDecimal 16 1 / BigDecimal 4 1 `shouldBe` BigDecimal 4 0
    it "yields x for x/1 for any x" $
      property $ \x -> x/1 === (x :: BigDecimal)
    it "yields 1 for x/x any non-zero x" $
      property $ \x -> if x /= (0 :: BigDecimal) then x / x === 1 else 1===1
    it "throws an Arithmetic exception when dividing by 0" $
      property $ \bd -> evaluate (bd / 0 :: BigDecimal) `shouldThrow` anyArithException
    it "yields y for (x*y)/x for any nonzero x" $
      property $ \x y -> y === if x == (0 :: BigDecimal) then y else (x*y)/x
    it "rounds up if next decimal would be > 5" $
      6 / 9 `shouldBe` fromString "0.6667"
    it "rounds up if next decimal would be = 5" $
      5 / 9 `shouldBe` fromString "0.5556"
    it "rounds down if next decimal would be < 5" $
      4 / 9 `shouldBe` fromString "0.4444"

  describe "fromRational" $ do
    it "constructs a BigDecimal from a Ratio" $
      fromRational (1 :% 32) `shouldBe` 1 / BigDecimal 32 0
    it "works for any non-zero divisors" $
      property $ \x y -> if y == 0 then 1 ===1 else fromRational (x :% y) === BigDecimal x 0 / BigDecimal y 0

  describe "toRational" $ do
    it "converts a BigDecimal to a Ratio" $
      toRational (1 / BigDecimal 32 0) `shouldBe` (1 :% 32)
    it "is inverse to fromRational" $
      property $ \x -> (x::BigDecimal) === fromRational (toRational x)

  describe "divide +/+" $ do
    -- checking expresions >= 0
    it "divides BigDecimals applying RoundingMode and precision" $
      divide (2, 3) (HALF_UP, Just 9) `shouldBe` fromString "0.666666667"
    it "always rounds down when using DOWN" $
      divide (2, 3) (DOWN, Just 9) `shouldBe` fromString "0.666666666"
    it "always rounds down when using FLOOR" $
      divide (2, 3) (FLOOR, Just 9) `shouldBe` fromString "0.666666666"
    it "rounds up when using UP when there is a remainder" $
      divide (1, 9) (UP, Just 3) `shouldBe` fromString "0.112"
    it "does not round when there is no remainder when using UP" $
      divide (14, 100) (UP, Just 2) `shouldBe` fromString "0.14"
    it "rounds up when using UP when there is a remainder" $
      divide (1, 9) (CEILING, Just 3) `shouldBe` fromString "0.112"
    it "does not round when there is no remainder when using UP" $
      divide (14, 100) (CEILING, Just 2) `shouldBe` fromString "0.14"
    it "rounds down if next decimal would be <= 5 when using HALF_DOWN" $
      divide (5, 9) (HALF_DOWN, Just 4) `shouldBe` fromString "0.5555"
    it "rounds up if next decimal would be > 5 when using HALF_DOWN" $
      divide (2, 3) (HALF_DOWN, Just 4) `shouldBe` fromString "0.6667"
    it "rounds up if next decimal would be >= 5 when using HALF_UP" $
      divide (5, 9) (HALF_UP, Just 4) `shouldBe` fromString "0.5556"
    it "rounds to next even number if next decimal would be == 5 when using HALF_EVEN" $
      divide (5, 9) (HALF_EVEN, Just 4) `shouldBe` fromString "0.5556"
    it "rounds to next even number if next decimal would be == 5 when using HALF_EVEN" $
      divide (1, 8) (HALF_EVEN, Just 2) `shouldBe` fromString "0.12"
    it "rounds to next even number if next decimal would be == 5 when using HALF_EVEN" $
      divide (15, 100) (HALF_EVEN, Just 1) `shouldBe` fromString "0.2"
    it "rounds up if next decimal would be > 5 when using HALF_EVEN" $
      divide (2, 3) (HALF_EVEN, Just 4) `shouldBe` fromString "0.6667"
    it "throws an exception when PRECISE is used and a non-terminating decimal expansion is detected" $
      evaluate (divide (5, 9) (PRECISE, Nothing)) `shouldThrow` anyException
    it "gives a precise value when using PRECISE and no max precision" $
      divide (1, 32) (PRECISE, Nothing) `shouldBe` fromString "0.03125"
    it "gives a precise value when using PRECISE and a sufficient precision" $
      divide (1, 32) (PRECISE, Just 5) `shouldBe` fromString "0.03125"
    it "gives a precise value when using PRECISE and a to small precision" $
      evaluate (divide (1, 32) (PRECISE, Just 4)) `shouldThrow` anyException

  describe "divide -/+" $ do
    -- checking dividend < 0
    it "divides BigDecimals applying RoundingMode and precision" $
      divide (-2, 3) (HALF_UP, Just 9) `shouldBe` fromString "-0.666666667"
    it "always rounds down when using DOWN" $
      divide (-2, 3) (DOWN, Just 9) `shouldBe` fromString "-0.666666666"
    it "always rounds towards -INF when using FLOOR" $
      divide (-2, 3) (FLOOR, Just 9) `shouldBe` fromString "-0.666666667"
    it "rounds up when using UP when there is a remainder" $
      divide (-1, 9) (UP, Just 3) `shouldBe` fromString "-0.112"
    it "does not round when there is no remainder when using UP" $
      divide (-14, 100) (UP, Just 2) `shouldBe` fromString "-0.14"
    it "rounds towards +INF when using CEILING when there is a remainder" $
      divide (-1, 9) (CEILING, Just 3) `shouldBe` fromString "-0.111"
    it "does not round when there is no remainder when using UP" $
      divide (-14, 100) (CEILING, Just 2) `shouldBe` fromString "-0.14"
    it "rounds down if next decimal would be <= 5 when using HALF_DOWN" $
      divide (-5, 9) (HALF_DOWN, Just 4) `shouldBe` fromString "-0.5555"
    it "rounds up if next decimal would be > 5 when using HALF_DOWN" $
      divide (-2, 3) (HALF_DOWN, Just 4) `shouldBe` fromString "-0.6667"
    it "rounds up if next decimal would be >= 5 when using HALF_UP" $
      divide (-5, 9) (HALF_UP, Just 4) `shouldBe` fromString "-0.5556"
    it "rounds to next even number if next decimal would be == 5 when using HALF_EVEN" $
      divide (-5, 9) (HALF_EVEN, Just 4) `shouldBe` fromString "-0.5556"
    it "rounds to next even number if next decimal would be == 5 when using HALF_EVEN" $
      divide (-1, 8) (HALF_EVEN, Just 2) `shouldBe` fromString "-0.12"
    it "rounds to next even number if next decimal would be == 5 when using HALF_EVEN" $
      divide (-15, 100) (HALF_EVEN, Just 1) `shouldBe` fromString "-0.2"
    it "rounds up if next decimal would be > 5 when using HALF_EVEN" $
      divide (-2, 3) (HALF_EVEN, Just 4) `shouldBe` fromString "-0.6667"
    it "throws an exception when PRECISE is used and a non-terminating decimal expansion is detected" $
      evaluate (divide (-5, 9) (PRECISE, Nothing)) `shouldThrow` anyException
    it "gives a precise value when using PRECISE and no max precision" $
      divide (-1, 32) (PRECISE, Nothing) `shouldBe` fromString "-0.03125"
    it "gives a precise value when using PRECISE and a sufficient precision" $
      divide (-1, 32) (PRECISE, Just 5) `shouldBe` fromString "-0.03125"
    it "gives a precise value when using PRECISE and a to small precision" $
      evaluate (divide (-1, 32) (PRECISE, Just 4)) `shouldThrow` anyException

  describe "divide +/-" $ do
    -- checking divisor < 0
    it "divides BigDecimals applying RoundingMode and precision" $
      divide (2, -3) (HALF_UP, Just 9) `shouldBe` fromString "-0.666666667"
    it "always rounds down when using DOWN" $
      divide (2, -3) (DOWN, Just 9) `shouldBe` fromString "-0.666666666"
    it "always rounds towards -INF when using FLOOR" $
      divide (2, -3) (FLOOR, Just 9) `shouldBe` fromString "-0.666666667"
    it "rounds up when using UP when there is a remainder" $
      divide (1, -9) (UP, Just 3) `shouldBe` fromString "-0.112"
    it "does not round when there is no remainder when using UP" $
      divide (14, -100) (UP, Just 2) `shouldBe` fromString "-0.14"
    it "rounds towards +INF when using CEILING when there is a remainder" $
      divide (1, -9) (CEILING, Just 3) `shouldBe` fromString "-0.111"
    it "does not round when there is no remainder when using UP" $
      divide (14, -100) (CEILING, Just 2) `shouldBe` fromString "-0.14"
    it "rounds down if next decimal would be <= 5 when using HALF_DOWN" $
      divide (5, -9) (HALF_DOWN, Just 4) `shouldBe` fromString "-0.5555"
    it "rounds up if next decimal would be > 5 when using HALF_DOWN" $
      divide (2, -3) (HALF_DOWN, Just 4) `shouldBe` fromString "-0.6667"
    it "rounds up if next decimal would be >= 5 when using HALF_UP" $
      divide (5, -9) (HALF_UP, Just 4) `shouldBe` fromString "-0.5556"
    it "rounds to next even number if next decimal would be == 5 when using HALF_EVEN" $
      divide (5, -9) (HALF_EVEN, Just 4) `shouldBe` fromString "-0.5556"
    it "rounds to next even number if next decimal would be == 5 when using HALF_EVEN" $
      divide (1, -8) (HALF_EVEN, Just 2) `shouldBe` fromString "-0.12"
    it "rounds to next even number if next decimal would be == 5 when using HALF_EVEN" $
      divide (15, -100) (HALF_EVEN, Just 1) `shouldBe` fromString "-0.2"
    it "rounds up if next decimal would be > 5 when using HALF_EVEN" $
      divide (2, -3) (HALF_EVEN, Just 4) `shouldBe` fromString "-0.6667"
    it "throws an exception when PRECISE is used and a non-terminating decimal expansion is detected" $
      evaluate (divide (5, -9) (PRECISE, Nothing)) `shouldThrow` anyException
    it "gives a precise value when using PRECISE and no max precision" $
      divide (1, -32) (PRECISE, Nothing) `shouldBe` fromString "-0.03125"
    it "gives a precise value when using PRECISE and a sufficient precision" $
      divide (1, -32) (PRECISE, Just 5) `shouldBe` fromString "-0.03125"
    it "gives a precise value when using PRECISE and a to small precision" $
      evaluate (divide (1, -32) (PRECISE, Just 4)) `shouldThrow` anyException

  describe "divide -/-" $ do
    -- checking dividend and divisor < 0
    it "divides BigDecimals applying RoundingMode and precision" $
      divide (-2, -3) (HALF_UP, Just 9) `shouldBe` fromString "0.666666667"
    it "always rounds down when using DOWN" $
      divide (-2, -3) (DOWN, Just 9) `shouldBe` fromString "0.666666666"
    it "always rounds towards -INF when using FLOOR" $
      divide (-2, -3) (FLOOR, Just 9) `shouldBe` fromString "0.666666666"
    it "rounds up when using UP when there is a remainder" $
      divide (-1, -9) (UP, Just 3) `shouldBe` fromString "0.112"
    it "does not round when there is no remainder when using UP" $
      divide (-14, -100) (UP, Just 2) `shouldBe` fromString "0.14"
    it "rounds towards +INF when using CEILING when there is a remainder" $
      divide (-1, -9) (CEILING, Just 3) `shouldBe` fromString "0.112"
    it "does not round when there is no remainder when using UP" $
      divide (-14, -100) (CEILING, Just 2) `shouldBe` fromString "0.14"
    it "rounds down if next decimal would be <= 5 when using HALF_DOWN" $
      divide (-5, -9) (HALF_DOWN, Just 4) `shouldBe` fromString "0.5555"
    it "rounds up if next decimal would be > 5 when using HALF_DOWN" $
      divide (-2, -3) (HALF_DOWN, Just 4) `shouldBe` fromString "0.6667"
    it "rounds up if next decimal would be >= 5 when using HALF_UP" $
      divide (-5, -9) (HALF_UP, Just 4) `shouldBe` fromString "0.5556"
    it "rounds to next even number if next decimal would be == 5 when using HALF_EVEN" $
      divide (-5, -9) (HALF_EVEN, Just 4) `shouldBe` fromString "0.5556"
    it "rounds to next even number if next decimal would be == 5 when using HALF_EVEN" $
      divide (-1, -8) (HALF_EVEN, Just 2) `shouldBe` fromString "0.12"
    it "rounds to next even number if next decimal would be == 5 when using HALF_EVEN" $
      divide (-15, -100) (HALF_EVEN, Just 1) `shouldBe` fromString "0.2"
    it "rounds up if next decimal would be > 5 when using HALF_EVEN" $
      divide (-2, -3) (HALF_EVEN, Just 4) `shouldBe` fromString "0.6667"
    it "throws an exception when PRECISE is used and a non-terminating decimal expansion is detected" $
      evaluate (divide (-5, -9) (PRECISE, Nothing)) `shouldThrow` anyException
    it "gives a precise value when using PRECISE and no max precision" $
      divide (-1, -32) (PRECISE, Nothing) `shouldBe` fromString "0.03125"
    it "gives a precise value when using PRECISE and a sufficient precision" $
      divide (-1, -32) (PRECISE, Just 5) `shouldBe` fromString "0.03125"
    it "gives a precise value when using PRECISE and a to small precision" $
      evaluate (divide (-1, -32) (PRECISE, Just 4)) `shouldThrow` anyException


  describe "shrink" $ do
    it "removes trailing zeros while taking care of the scale" $
      shrink 0 (BigDecimal 1000 3) `shouldBe` BigDecimal 1 0
    it "does not eliminate more 0s than requested" $
      shrink 2 (BigDecimal 1000 3) `shouldBe` BigDecimal 100 2
    it "does not eliminate more 0s than possible" $
      shrink 0 (BigDecimal 1230 3) `shouldBe` BigDecimal 123 2

  describe "matchScales" $
    it "adjusts a pair of BigDecimals to use the same scale" $
      property $ \x y -> let (x', y') = matchScales (x,y) in getScale x' === getScale y'

  describe "roundBD" $ do
    it "rounds a BigDecimal " $
      roundBD (BigDecimal 123456 3) (halfUp 2) `shouldBe` BigDecimal 12346 2
    it "ignores negative scales in MathContext" $
      roundBD (BigDecimal 123456 3) (halfUp (-2)) `shouldBe` BigDecimal 123456 3
    it "ignores MathContext with scale higher than in input value" $
      roundBD (BigDecimal 123456 3) (halfUp 10) `shouldBe` BigDecimal 123456 3

  -- mathematical functions on BigDecimals
  describe "sqr" $ do
    it "computes the square root of any non-negative BigDecimal" $
      property $ \x scale -> let (x', r) = (abs x, sqr x' $ halfUp scale) in abs (r*r - x') < BigDecimal 1000 scale
    it "throws an exception if applied to a negative number" $
      evaluate (sqr (-16) $ halfUp 2) `shouldThrow` anyException

  -- mathematical functions on BigDecimals
  describe "nthRoot" $
    it "computes the nth root of any non-negative BigDecimal" $
      property $ \x n -> let (x', n', r) = (1+ abs x, 1+abs n, nthRoot x' n' (halfUp 10)) in abs (r^n' - x') < BigDecimal (n'*10000) 10
--    it "throws an exception if applied to a negative number" $
--      evaluate (sqr (-16) $ halfUp 2) `shouldThrow` anyException
