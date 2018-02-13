module BigDecimal where

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
    a / b = divi $ matchDigits (a ,b)
    fromRational = undefined

-- | add two BigDecimals with same precision
plus :: (BigDecimal, BigDecimal) -> BigDecimal
plus (BigDecimal integerA scaleA, BigDecimal integerB scaleB)
    = BigDecimal (integerA + integerB) scaleA

-- | multiply two BigDecimals    
mul :: (BigDecimal, BigDecimal) -> BigDecimal
mul (BigDecimal integerA scaleA, BigDecimal integerB scaleB)
    = BigDecimal (integerA * integerB) (scaleA + scaleB)

-- | divide two BigDecimals
divi :: (BigDecimal, BigDecimal) -> BigDecimal
divi (BigDecimal numA digitsA, BigDecimal numB digitsB) =
    let maxPrecision = undefined --round $ 10 * precision numB / 3
    in
      BigDecimal
        (round $ fromInteger numA / fromInteger numB *10^maxPrecision)
        maxPrecision

-- | match the scales of a tuple of BigDecimals
matchDigits :: (BigDecimal, BigDecimal) -> (BigDecimal, BigDecimal)
matchDigits (a@(BigDecimal integerA scaleA), b@(BigDecimal integerB scaleB))
    | scaleA < scaleB = (BigDecimal (integerA * 10^(scaleB-scaleA)) scaleB, b)
    | scaleA > scaleB = (a, BigDecimal (integerB * 10^(scaleA-scaleB)) scaleA)
    | otherwise       = (a, b)

--
precision :: Integer -> Integer
precision 0    = 1
precision val  = 1 + floor (logBase 10 $ fromInteger val)



a = BigDecimal 1234 2
b = BigDecimal 5678 3
ad = 12.34
bd = 5.678

one = BigDecimal 1 0
thirtyTwo = BigDecimal 32 0