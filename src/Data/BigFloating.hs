{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.BigFloating
  ( piChudnovsky
  , pI
  , sqr
  , nthRoot
  , BigFloating (..)
  , P100
  , P500
  )
where

import           Data.BigDecimal
import           Data.List  (find)
import           Data.Maybe (fromMaybe, fromJust)
import           GHC.Real   ((%), Ratio ((:%)))
import           GHC.Natural
import           Data.Fixed (HasResolution(..))
import           Data.Data 

-- | The type parameter should be an instance of 'HasResolution'.
newtype BigFloating (a :: k) = BF BigDecimal deriving (Show, Read, Eq, Ord)

data P100
instance HasResolution P100 where
    resolution _ = 100

data P500
instance HasResolution P500 where
    resolution _ = 500

instance (HasResolution a) => Num (BigFloating a) where
  (BF a) + (BF b) = BF $ a + b
  (BF a) * (BF b) = BF $ a * b
  abs (BF a)      = BF (abs a)
  signum (BF a)   = BF (signum a)
  fromInteger     = BF . fromInteger
  negate (BF a)   = BF (negate a)
  
instance (HasResolution a) => Fractional (BigFloating a) where
  fromRational = BF . fromRational
  recip (BF a) = BF (recip a)
  
instance (HasResolution a) => Floating (BigFloating a) where
  pi = BF $ piChudnovsky (DOWN, Just $ fromInteger res)
        where res = resolution (Proxy :: Proxy a)
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

-- I'm giving some implementation ideas for approximisations for functions on transcendental numbers.
-- The rest is left as an exercise to the interested reader ;-)
instance Floating BigDecimal where
    pi    = piChudnovsky defaultRounding
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
    sqrt x = sqr x defaultRounding
    x ** y = nthRoot (x^b) (fromIntegral n) defaultRounding
                where
                  (b :% n) = toRational y

defaultRounding :: RoundingAdvice
defaultRounding = (DOWN, Just 100)

-- | computes the square root of any non-negative BigDecimal, rounding and precision defined by RoundingAdvice.
--   We are using Newton's algorithm.
sqr :: BigDecimal -> RoundingAdvice -> BigDecimal
sqr x mc
  | x <  0    = error "can't determine the square root of negative numbers"
  | x == 0    = 0
  | otherwise = fst $ fromMaybe (error "did not find a sqrt") $ refine x 1 mc
      where
        refine _ _ (_, Nothing)           = error "can't produce square root with unlimited precision"
        refine r initial ra@(_, Just scl) = find withinPrecision $ iterate nextGuess (initial, 0)
          where
            withinPrecision (guess, count) = abs (guess^(2::Int) - r) < bigDecimal 10 scl || count > 10 * scl * precision r
            nextGuess (guess, count) = (nf $ divide (guess + divide (r, guess) mc, 2) ra, count+1)

-- | computes the n-th root of BigDecimal x. rounding and precision defined by RoundingAdvice.
--   We are using Newton's algorithm.
nthRoot :: BigDecimal -> Natural -> RoundingAdvice -> BigDecimal
nthRoot x n mc@(rm, maybeScale)
  | x <  0 && even n   = error "can't determine even roots of negative numbers"
  | x <  0 && odd  n   = - nthRoot (-x) n mc
  | x == 0    = 0
  | otherwise = roundBD (fst (fromMaybe (error "did not find a sqrt") $ refine x 1 (rm, Just (s+4)))) mc
      where
        s  = fromJust maybeScale
        refine _ _ (_, Nothing)             = error "can't produce nth root with unlimited precision"
        refine r initial ra@(_, Just scl) = find withinPrecision $ iterate nextGuess (initial, 0)
          where
            withinPrecision (guess, count) = abs (guess^n - r) < bigDecimal (fromIntegral $ n*10) scl || count > 10 * scl * precision r
            nextGuess (guess, count) =
              (nf $ divide ((guess * bigDecimal (fromIntegral $ n-1) 0) + divide (r, guess^(n-1)) ra, bigDecimal (fromIntegral n) 0) ra, count+1)


-- | Compute pi using rounding mode and scale of the specified RoundingAdvice
--   Sources: https://wiki.haskell.org/Integers_too_big_for_floats & https://github.com/eobermuhlner/big-math
piChudnovsky :: RoundingAdvice -> BigDecimal
piChudnovsky (_, Nothing)           = error "can't compute pi with umlimited precision"
piChudnovsky mc@(rMode, Just scl) = divide (1, 12 * divide (fromRatio s mc,f) mc') mc
    where
      mc'   = (rMode, Just $ scl + 3) -- increase precision to avoid propagation of rounding errors
      steps = 1 + div scl  14         -- taken from github.com/eobermuhlner/big-math
      s = sum [chudnovsky (fromIntegral n) | n <- [0..steps]] :: Rational
      f = sqr (fromInteger c^(3::Int)) mc      -- Common factor in the sum

      -- k-th term of the Chudnovsky series
      chudnovsky :: Integer -> Rational
      chudnovsky k
          | even k    =  quotient
          | otherwise = -quotient
          where
            quotient = num % den
            num  = facDiv (6 * k) (3 * k) * (a + b * k)
            den  = fac k ^ (3::Int) * (c ^ (3 * k))

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

pI :: Int -> BigDecimal
pI n = fromString (concat (["3", "."] ++ map show (tail (take n piList))))


-- This is R. Zumkellers algorithm taken from https://oeis.org/search?q=pi&sort=&language=&go=Search.
--piList :: Integral a => [a]
piList :: [Integer]
piList = piStream (1, 0, 1)
   [(n, a*d, d) | (n, d, a) <- map (\k -> (k, 2 * k + 1, 2)) [1..]] where
   piStream z xs'@(x:xs)
     | lb /= approx z 4 = piStream (mult z x) xs
     | otherwise = lb : piStream (mult (10, -10 * lb, 1) z) xs'
     where lb = approx z 3
           approx (a, b, c) n = div (a * n + b) c
           mult (a, b, c) (d, e, f) = (a * d, a * e + b * f, c * f) 
   piStream (_,_,_) [] = error "should never happen" -- avoid pattern matching warning
