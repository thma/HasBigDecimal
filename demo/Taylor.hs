{-# LANGUAGE DeriveFunctor, DeriveFoldable, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
module Taylor where

-- playing around with ideas from https://iagoleal.com/posts/calculus-symbolic-ode/
-- my idea is to use the taylor series based definitions to give implementations for the BigDecimal Floating instance.

import           Data.BigDecimal
import           Data.Foldable (toList)
import           Data.Char (digitToInt, intToDigit)

defaultRounding :: RoundingAdvice
defaultRounding = (DOWN, Just 400)

factorial :: Integer -> BigDecimal
factorial n = fromIntegral $ product [1..n]       

e :: BigDecimal -> BigDecimal 
e x = foldr (\i r -> x ^ i / factorial i + r) 0 [0..100]
sin' :: BigDecimal -> BigDecimal 
sin' x =  foldr (\i r-> ((-1) ^ i / factorial (2*i+1)) * x^(2*i + 1) + r) 0 [0..10000]
cos' :: BigDecimal -> BigDecimal 
cos' x = foldr (\i r -> ((-1) ^ i / factorial (2*i)) * x^(2*i) + r) 0 [0..10000]

euler :: Int -> BigDecimal
euler n = fromString (concat (["2", "."] ++ map show (tail (take n eList))))


eList = eStream (1, 0, 1)
   [(n, a * d, d) | (n, d, a) <- map (\k -> (1, k, 1)) [1..]] where
   eStream z xs'@(x:xs)
     | lb /= approx z 2 = eStream (mult z x) xs
     | otherwise = lb : eStream (mult (10, -10 * lb, 1) z) xs'
     where lb = approx z 1
           approx (a, b, c) n = div (a * n + b) c
           mult (a, b, c) (d, e, f) = (a * d, a * e + b * f, c * f) 

-- pI :: Int -> BigDecimal
-- pI n = fromString (concat (["3", "."] ++ map show (tail (take n piList))))


-- piList :: Integral a => [a]
-- piList = piStream (1, 0, 1)
--    [(n, a*d, d) | (n, d, a) <- map (\k -> (k, 2 * k + 1, 2)) [1..]] where
--    piStream z xs'@(x:xs)
--      | lb /= approx z 4 = piStream (mult z x) xs
--      | otherwise = lb : piStream (mult (10, -10 * lb, 1) z) xs'
--      where lb = approx z 3
--            approx (a, b, c) n = div (a * n + b) c
--            mult (a, b, c) (d, e, f) = (a * d, a * e + b * f, c * f) 

data Stream a = a :> Stream a
  deriving (Functor, Foldable)

infixr 2 :>

ex :: Num a => Stream a
ex = 1 :> ex

sine :: Num a => Stream a
sine   = 0 :> cosine

cosine :: Num a => Stream a
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

eval' :: BigDecimal -> Stream BigDecimal -> BigDecimal -> BigDecimal
eval' a f x = foldr1 (\ fa f' -> fa + (x - a) * f') (take 5000 taylor)
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

analytic :: Num a => (a -> a) -> (Stream a -> Stream a) -> Stream a -> Stream a
analytic g g' f@(fa :> f') = g fa :> g' f * f'

-- instance Floating (Stream BigDecimal) where
--   pi    = piChudnovsky defaultRounding :> zero
--   exp   = analytic exp   exp
--   log   = analytic log   recip
--   sin   = analytic sin   cos
--   cos   = analytic cos   (negate . sin)
--   asin  = analytic asin  (\x -> 1 / sqrt (1 - x^2))
--   acos  = analytic acos  (\x -> -1 / sqrt (1 - x^2))
--   atan  = analytic atan  (\x -> 1 / (1 + x^2))
--   sinh  = analytic sinh  cosh
--   cosh  = analytic cosh  sinh
--   asinh = analytic asinh (\x -> 1 / sqrt (x^2 + 1))
--   acosh = analytic acosh (\x -> 1 / sqrt (x^2 - 1))
--   atanh = analytic atanh (\x -> 1 / (1 - x^2))



