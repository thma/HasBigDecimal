module Data.BigDecimal where

import           Data.List
import           Data.Maybe (fromMaybe)
import           GHC.Real   (Ratio ((:%)))

data RoundingMode
  = UP        -- Rounding mode to round away from zero.
  | DOWN      -- Rounding mode to round towards zero.
  | CEILING   -- Rounding mode to round towards positive infinity.
  | FLOOR     -- Rounding mode to round towards negative infinity.
  | HALF_UP   -- Rounding mode to round towards "nearest neighbor" unless both neighbors are equidistant, in which case round up.
  | HALF_DOWN -- Rounding mode to round towards "nearest neighbor" unless both neighbors are equidistant, in which case round down.
  | HALF_EVEN -- Rounding mode to round towards "nearest neighbor" unless both neighbors are equidistant, in which case, round towards the even neighbor.
  | PRECISE   -- Rounding mode to assert that the requested operation has an exact result, hence no rounding is applied.

-- | BigDecimal is represented by an unscaled Integer value plus a second Integer value that defines the scale
--   E.g.: (BigDecimal 1234 2) represents the decimal value 12.34
data BigDecimal =
  BigDecimal Integer Integer
  deriving (Show, Read)

-- gets the scale part of a BigDecimal
getScale :: BigDecimal -> Integer
getScale (BigDecimal _ s) = s

-- get the unscaled value of a BigDecimal
getValue :: BigDecimal -> Integer
getValue (BigDecimal v _) = v

-- | MathContext is a pair of a RoundingMode and a target scale.
type MathContext = (RoundingMode, Maybe Integer)

instance Num BigDecimal where
  a + b = plus $ matchScales (a, b)
  a * b = mul (a, b)
  abs (BigDecimal v s) = BigDecimal (abs v) s
  signum (BigDecimal v _) = BigDecimal (signum v) 0
  fromInteger i = BigDecimal i 0
  negate (BigDecimal v s) = BigDecimal (-v) s

instance Eq BigDecimal where
  a == b =
    let (BigDecimal valA _, BigDecimal valB _) = matchScales (a, b)
    in valA == valB

instance Fractional BigDecimal where
  -- default division rounds up and does not limit precision
  a / b = divide (matchScales (a, b)) (HALF_UP, Nothing)
  fromRational (x :% y) = BigDecimal x 0 / BigDecimal y 0

instance Real BigDecimal where
  toRational (BigDecimal val scale) = toRational val * 10^^(-scale)

instance Ord BigDecimal where
  compare a b =
    let (BigDecimal valA _, BigDecimal valB _) = matchScales (a, b)
    in compare valA valB

-- instance Floating BigDecimal where
-- this is left as an exercise to the reader ;-)

-- | add two BigDecimals with same scale
plus :: (BigDecimal, BigDecimal) -> BigDecimal
plus (a@(BigDecimal valA scaleA), b@(BigDecimal valB scaleB))
  | scaleA == scaleB = BigDecimal (valA + valB) scaleA
  | otherwise        = plus $ matchScales (a,b)

-- | multiply two BigDecimals
mul :: (BigDecimal, BigDecimal) -> BigDecimal
mul (BigDecimal valA scaleA, BigDecimal valB scaleB) = BigDecimal (valA * valB) (scaleA + scaleB)

-- | divide two BigDecimals with applying the RoundingMode and the specified precision
-- | if Nothing if given as precision the maximum possible precision is used
divide :: (BigDecimal, BigDecimal) -> MathContext -> BigDecimal
divide (a, b) (rMode, prefScale) =
  let (BigDecimal numA _, BigDecimal numB _) = matchScales (a, b)
      maxPrecision = fromMaybe (precision numA + round (fromInteger (precision numB) * 10 / 3)) prefScale
  in shrink maxPrecision (BigDecimal (divUsing rMode (numA * (10 :: Integer) ^ maxPrecision) numB) maxPrecision)

-- | divide two correctly scaled Integers and apply the RoundingMode
divUsing :: RoundingMode -> Integer -> Integer -> Integer
divUsing rounding a b =
  let (quot, rem) = quotRem a b
      delta = (10 * abs rem `div` abs b) - 5
  in case rounding of
       PRECISE   -> if rem == 0    then quot else error "non-terminating decimal expansion"
       UP        -> if abs rem > 0 then quot + signum quot else quot
       CEILING   -> if abs rem > 0 && quot >= 0 then quot + 1 else quot
       HALF_UP   -> if delta >= 0  then quot + signum quot else quot
       HALF_DOWN -> if delta <= 0  then quot else quot + signum quot
       DOWN      -> quot
       FLOOR     -> if quot >= 0   then quot else quot - 1
       HALF_EVEN
         | delta > 0              -> quot + signum quot
         | delta == 0 && odd quot -> quot + signum quot
         | otherwise              -> quot


-- | round a BigDecimal to `n` digits applying rounding mode
roundBD :: BigDecimal -> RoundingMode -> Integer -> BigDecimal
roundBD bd@(BigDecimal val scale) rMode n
  | n < 0 || n >= scale = bd
  | otherwise           = BigDecimal (divUsing rMode val (10 ^ (scale-n))) n

-- | match the scales of a tuple of BigDecimals
matchScales :: (BigDecimal, BigDecimal) -> (BigDecimal, BigDecimal)
matchScales (a@(BigDecimal integerA scaleA), b@(BigDecimal integerB scaleB))
  | scaleA < scaleB = (BigDecimal (integerA * 10 ^ (scaleB - scaleA)) scaleB, b)
  | scaleA > scaleB = (a, BigDecimal (integerB * 10 ^ (scaleA - scaleB)) scaleA)
  | otherwise = (a, b)

-- | returns the number of digits of an Integer
precision :: Integer -> Integer
precision 0   = 1
precision val = 1 + floor (logBase 10 $ abs $ fromInteger val)

-- | removes trailing zeros from a BigDecimals intValue by decreasing the scale
shrink :: Integer -> BigDecimal -> BigDecimal
shrink prefScale bd@(BigDecimal val scale) =
  let (v, r) = quotRem val 10
  in if r == 0 && 0 < prefScale && prefScale < scale
       then shrink prefScale $ BigDecimal v (scale - 1)
       else bd

-- | read a BigDecimal from a human readable decimal notation
fromString :: String -> BigDecimal
fromString s =
  let maybeIndex = elemIndex '.' s
      intValue = read (filter (/= '.') s) :: Integer
  in case maybeIndex of
       Nothing -> BigDecimal intValue 0
       Just i  -> BigDecimal intValue $ toInteger (length s - i - 1)

-- | returns a readable String representation
toString :: BigDecimal -> String
toString bd@(BigDecimal intValue scale) =
  let s = show $ abs intValue
      filled =
        if fromInteger scale >= length s
          then replicate (1 + fromInteger scale - length s) '0' ++ s
          else s
      splitPos = length filled - fromInteger scale
      (ints, decimals) = splitAt splitPos filled
      sign = if intValue < 0 then "-" else ""
  in sign ++ if not (null decimals) then ints ++ "." ++ decimals else ints

sqrt :: BigDecimal -> MathContext -> BigDecimal
sqrt bd mc@(rounding, Just scale) = undefined

sqr :: Double -> Maybe Double
sqr x = refineSqrtGuess x (x/2)

refineSqrtGuess :: Double -> Double -> Maybe Double
refineSqrtGuess x initial = find withinPrecision $ iterate newtons initial
    where
          withinPrecision guess = abs (guess^2 - x) < 0.001 * x
          newtons guess = (guess + x / guess) / 2


