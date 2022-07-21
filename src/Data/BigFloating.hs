{-# LANGUAGE DeriveFunctor, DeriveFoldable, NoMonomorphismRestriction #-}
module Data.BigFloating
  ( piChudnovsky
  , sqr
  , nthRoot
  )
where

import           Data.BigDecimal
import           Data.List  (find)
import           Data.Maybe (fromMaybe, fromJust)
import           GHC.Real   ((%), Ratio ((:%)))
import           GHC.Natural
import           Data.Foldable (toList)

data Stream a = a :> Stream a
  deriving (Functor, Foldable)

infixr 2 :>

ex :: Num a => Stream a
ex = 1 :> ex

sine   = 0 :> cosine
cosine = 1 :> fmap negate sine


-- | Turn a Stream f into a functional approximation
--   of its Taylor series around a point a.
-- That is, eval a f ≈ f(a + x)
eval :: Fractional a => a -> Stream a -> a -> a
eval a f x = foldr1 (\ fa f' -> fa + (x - a) * f') (take 300 taylor)
 where
  taylor      = zipWith (/) (toList f) factorials
  factorials  = let fats = 1 : zipWith (*) fats [1..]
                in fmap fromIntegral fats

-- | Taylor series representation of the derivative.
diff :: Stream a -> Stream a
diff (_ :> f') = f'

-- | Taylor series for the constant zero.
zero :: Num a => Stream a
zero = 0 :> zero

euler :: BigDecimal
euler = eval 0 ex 1 

p :: BigDecimal
p = eval 0 pi 0

-- | Taylor series for the identity function `f x = x`.
x :: Num a => Stream a
x = 0 :> 1 :> zero

instance Num a => Num (Stream a) where
  -- Good ol' linearity
  (+)  (fa :> f')  (ga :> g') = fa + ga :> f' + g'
  (-)  (fa :> f')  (ga :> g') = fa - ga :> f' - g'
  negate = fmap negate
  -- Leibniz rule applied to streams
  (*) f@(fa :> f') g@(ga :> g') = fa * ga :> f' * g + f * g'
  fromInteger n = fromInteger n :> zero
  abs    = error "Absolute value is not a smooth function"
  signum = error "No well-defined sign for a series"

instance Fractional a => Fractional (Stream a) where
  -- The division rule from Calculus. We assume g(0) ≠ 0
  (/) f@(fa :> f') g@(ga :> g') = fa / ga :> (f' * g - f * g') / g^2
  fromRational n = fromRational n :> zero

analytic g g' f@(fa :> f') = g fa :> g' f * f'

instance Floating a => Floating (Stream a) where
  pi    = pi :> zero
  exp   = analytic exp   exp
  log   = analytic log   recip
  sin   = analytic sin   cos
  cos   = analytic cos   (negate . sin)
  asin  = analytic asin  (\x -> 1 / sqrt (1 - x^2))
  acos  = analytic acos  (\x -> -1 / sqrt (1 - x^2))
  atan  = analytic atan  (\x -> 1 / (1 + x^2))
  sinh  = analytic sinh  cosh
  cosh  = analytic cosh  sinh
  asinh = analytic asinh (\x -> 1 / sqrt (x^2 + 1))
  acosh = analytic acosh (\x -> 1 / sqrt (x^2 - 1))
  atanh = analytic atanh (\x -> 1 / (1 - x^2))

 



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
        refine x initial mc@(_, Just scale) = find withinPrecision $ iterate nextGuess (initial, 0)
          where
            withinPrecision (guess, count) = abs (guess^2 - x) < BigDecimal 10 scale || count > 10 * scale * fromIntegral (precision x)
            nextGuess (guess, count) = (nf $ divide (guess + divide (x, guess) mc, 2) mc, count+1)

nthRoot :: BigDecimal -> Natural -> RoundingAdvice -> BigDecimal
nthRoot x n mc@(r, maybeScale)
  | x <  0 && even n   = error "can't determine even roots of negative numbers"
  | x <  0 && odd  n   = - nthRoot x (-n) mc
  | x == 0    = 0
  | otherwise = roundBD (fst (fromMaybe (error "did not find a sqrt") $ refine x 1 (r, Just (s+4)))) mc
      where
        s  = fromJust maybeScale
        refine x initial mc@(_, Just scale) = find withinPrecision $ iterate nextGuess (initial, 0)
          where
            withinPrecision (guess, count) = abs (guess^n - x) < BigDecimal (fromIntegral $ n*10) scale || count > 10 * scale * precision x
            nextGuess (guess, count) =
              (nf $ divide ((guess * BigDecimal (fromIntegral $ n-1) 0) + divide (x, guess^(n-1)) mc, BigDecimal (fromIntegral n) 0) mc, count+1)


-- | Compute pi using rounding mode and scale of the specified RoundingAdvice
--   Sources: https://wiki.haskell.org/Integers_too_big_for_floats & https://github.com/eobermuhlner/big-math
piChudnovsky :: RoundingAdvice -> BigDecimal
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


