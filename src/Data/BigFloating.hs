module Data.BigFloating
  ( piChudnovsky
  , sqr
  , nthRoot
  )
where

import Data.BigDecimal
import           Data.List  (find)
import           Data.Maybe (fromMaybe)
import           GHC.Real   ((%), Ratio ((:%)))

-- I'm giving some implementation ideas for approximisations for functions on transcendental numbers.
-- The rest is left as an exercise to the interested reader ;-)
instance Floating BigDecimal where
    pi    = piChudnovsky defaultMC
    exp   = undefined -- e^x
    log   = undefined
    sin   = undefined
    cos   = undefined
    asin  = undefined
    acos  = undefined
    atan  = undefined
    sinh  = undefined
    cosh  = undefined
    asinh = undefined
    acosh = undefined
    atanh = undefined

-- not required for minimal implementation
    sqrt x = sqr x defaultMC
    x ** y = nthRoot (x^b) n defaultMC
                where
                  (b :% n) = toRational y

defaultMC = (DOWN, Just 100)

-- | computes the square root of any non-negative BigDecimal, rounding and precision defined by MathContext.
--   We are using Newton's algorithm.
sqr :: BigDecimal -> MathContext -> BigDecimal
sqr x mc
  | x <  0    = error "can't determine the square root of negative numbers"
  | x == 0    = 0
  | otherwise = fst $ fromMaybe (error "did not find a sqrt") $ refine x 1 mc
      where
        refine x initial mc@(_, Just scale) = find withinPrecision $ iterate nextGuess (initial, 0)
          where
            withinPrecision (guess, count) = abs (guess^2 - x) < bigDecimal 10 scale || count > 10 * scale * (fromIntegral $ precision x)
            nextGuess (guess, count) = (nf $ divide (guess + divide (x, guess) mc, 2) mc, count+1)

nthRoot :: BigDecimal -> Integer -> MathContext -> BigDecimal
nthRoot x n mc@(r,Just s)
  | x <  0 && even n   = error "can't determine even roots of negative numbers"
  | x <  0 && odd  n   = - nthRoot x (-n) mc
  | x == 0    = 0
  | otherwise = roundBD (fst (fromMaybe (error "did not find a sqrt") $ refine x 1 (r, Just (s+4)))) mc
      where
        refine x initial mc@(_, Just scale) = find withinPrecision $ iterate nextGuess (initial, 0)
          where
            withinPrecision (guess, count) = abs (guess^n - x) < bigDecimal (n*10) scale || count > 10 * scale * precision x
            nextGuess (guess, count) =
              (nf $ divide ((guess * bigDecimal (n-1) 0) + divide (x, guess^(n-1)) mc, bigDecimal n 0) mc, count+1)


-- | Compute pi using rounding mode and scale of the specified MathContext
--   Sources: https://wiki.haskell.org/Integers_too_big_for_floats & https://github.com/eobermuhlner/big-math
piChudnovsky :: MathContext -> BigDecimal
piChudnovsky mc@(rMode, Just scale) = divide (1, 12 * divide (fromRatio s mc,f) mc') mc
    where
      mc'   = (rMode, Just $ scale + 3) -- increase precision to avoid propagation of rounding errors
      steps = 1 + div scale  14         -- taken from github.com/eobermuhlner/big-math
      s = sum [chudnovsky (fromIntegral n) | n <- [0..steps]] :: Rational
      f = sqr (fromInteger c^3) mc      -- Common factor in the sum

      -- k-th term of the Chudnovsky series
      chudnovsky :: Integer -> Rational
      chudnovsky k
          | even k    =  quot
          | otherwise = -quot
          where
            quot = num % den
            num  = facDiv (6 * k) (3 * k) * (a + b * k)
            den  = fac k ^ 3 * (c ^ (3 * k))

      -- Compute n!
      fac :: (Enum a, Num a) => a -> a
      fac n = product [1..n]

      -- Compute n! / m! efficiently
      facDiv :: Integer -> Integer -> Integer
      facDiv n m
          | n > m     = product [n, n - 1 .. m + 1]
          | n == m    = 1
          | otherwise = facDiv m n

      a = 13591409
      b = 545140134
      c = 640320


