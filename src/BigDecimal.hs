module BigDecimal where

import Data.List
import Data.Maybe (fromMaybe)

data RoundingMode =
    ROUND_UP           -- Rounding mode to round away from zero.
  | ROUND_DOWN         -- Rounding mode to round towards zero.
  | ROUND_CEILING      -- Rounding mode to round towards positive infinity.
  | ROUND_FLOOR        -- Rounding mode to round towards negative infinity.
  | ROUND_HALF_UP      -- Rounding mode to round towards "nearest neighbor" unless both neighbors are equidistant, in which case round up.
  | ROUND_HALF_DOWN    -- Rounding mode to round towards "nearest neighbor" unless both neighbors are equidistant, in which case round down.
  | ROUND_HALF_EVEN    -- Rounding mode to round towards the "nearest neighbor" unless both neighbors are equidistant, in which case, round towards the even neighbor.
  | ROUND_UNNECESSARY  -- Rounding mode to assert that the requested operation has an exact result, hence no rounding is necessary.

data BigDecimal = BigDecimal Integer Integer deriving (Show, Read, Eq)
instance Num BigDecimal where
    a + b         = plus $ matchDigits (a, b)
    a * b         = mul (a, b)
    abs           = undefined
    signum        = undefined
    fromInteger i = BigDecimal i 0
    negate (BigDecimal num digits) = BigDecimal (-num) digits

instance Fractional BigDecimal where
    a / b = divide (matchDigits (a ,b)) ROUND_UP Nothing
    fromRational = undefined

-- | add two BigDecimals with same precision
plus :: (BigDecimal, BigDecimal) -> BigDecimal
plus (BigDecimal integerA scaleA, BigDecimal integerB scaleB)
    = BigDecimal (integerA + integerB) scaleA

-- | multiply two BigDecimals    
mul :: (BigDecimal, BigDecimal) -> BigDecimal
mul (BigDecimal integerA scaleA, BigDecimal integerB scaleB)
    = BigDecimal (integerA * integerB) (scaleA + scaleB)

-- | divide two BigDecimals with applying the RoundingMode and the specified Precicion
-- | if Nothing if given as precision the maximum possible precision is used
divide :: (BigDecimal, BigDecimal) -> RoundingMode -> Maybe Integer -> BigDecimal
divide (a, b) rMode prefScale =
    let
       (BigDecimal numA digitsA, BigDecimal numB digitsB) = matchDigits (a, b)
       maxPrecision =
          fromMaybe
            (precision numA + round (fromInteger (precision numB) * 10 / 3))
            prefScale
    in shrink maxPrecision
      (BigDecimal
        (divUsing rMode (numA * (10::Integer)^maxPrecision) numB)
        maxPrecision)

divUsing :: RoundingMode -> Integer -> Integer -> Integer
divUsing rMode a b =
  let quot = div a b
      rem  = mod a b
  in case rMode of
        ROUND_UNNECESSARY -> if rem == 0 then quot else error "non-terminating decimal expansion"
        _                 -> quot --round (fromInteger a / fromInteger b)

-- | match the scales of a tuple of BigDecimals
matchDigits :: (BigDecimal, BigDecimal) -> (BigDecimal, BigDecimal)
matchDigits (a@(BigDecimal integerA scaleA), b@(BigDecimal integerB scaleB))
    | scaleA < scaleB = (BigDecimal (integerA * 10^(scaleB-scaleA)) scaleB, b)
    | scaleA > scaleB = (a, BigDecimal (integerB * 10^(scaleA-scaleB)) scaleA)
    | otherwise       = (a, b)

--
precision :: Integer -> Integer
precision 0    = 1
precision val  = 1 + floor (logBase 10 $ abs $ fromInteger val)

-- removes trailing zeros from a BigDecimals intValue by decreasing the scale
shrink :: Integer -> BigDecimal -> BigDecimal
shrink prefScale bd@(BigDecimal val scale)  =
  let r = mod val 10
      v = div val 10
  in if r == 0 && 0 <= prefScale && prefScale < scale
       then shrink prefScale $ BigDecimal v (scale-1)
       else bd

toBD :: String -> BigDecimal
toBD s =
  let maybeIndex = elemIndex '.' s
      intValue   = read (filter (/= '.') s) :: Integer
  in case maybeIndex of
        Nothing -> BigDecimal intValue 0
        Just i  -> BigDecimal intValue $ toInteger (length s - i - 1)

toString :: BigDecimal -> String
toString bd@(BigDecimal intValue scale) =
  let s                = show intValue
      filled           = if fromInteger scale >= length s
                         then replicate (1 + fromInteger scale - length s) '0' ++ s
                         else s
      splitPos         = length filled - fromInteger scale
      (ints, decimals) = splitAt splitPos filled
  in
    if splitPos >= 0 then ints ++ "." ++ decimals
    else show splitPos

a = BigDecimal 1234 2
b = BigDecimal 5678 3
ad = 12.34
bd = 5.678

one = BigDecimal 1 0
three = BigDecimal 3 0
four = BigDecimal 4 0
five = BigDecimal 5 0
nine = BigDecimal 9 0
seven = toBD "7"
thirtyTwo = BigDecimal 32 0