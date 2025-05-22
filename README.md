[![Actions Status](https://github.com/thma/HasBigDecimal/workflows/Haskell-CI/badge.svg)](https://github.com/thma/HasBigDecimal/actions)

# HasBigDecimal

This module defines the type 'BigDecimal' which provides a representation of arbitrary precision decimal numbers.
'BigDecimal' is a native Haskell implementation based on arbitrary sized 'Integer' values.
The implementation was inspired by Java BigDecimals. It aims to provide a simple to use API.

```haskell
-- | BigDecimal is represented by an unscaled Integer value and a Natural that defines the scale
--   E.g.: (BigDecimal value = 1234, scale = 2) represents the decimal value 12.34.
data BigDecimal = BigDecimal
  { 
    value :: Integer,  -- ^ the unscaled Integer value    
    scale :: Natural   -- ^ the scale (i.e. the number of digits after the decimal point)
  }
```

BigDecimal instantiates the following typeclasses:

```haskell
instance Eq BigDecimal
instance Ord BigDecimal

instance Num BigDecimal
instance Fractional BigDecimal
instance Real BigDecimal

instance Read BigDecimal
instance Show BigDecimal
```

It is thus possible to use all common numerical operations on operators like '+', '-', '*', '/', '^' on them.


# Some examples from a ghci REPL
```haskell
import           Data.BigDecimal

main :: IO ()
main = do
  let a = read "3.1415926"
      b = bigDecimal 31415926 7
      c = 3.141592653589793238462643383279502884197 :: BigDecimal
      d = 3.141592653589793238462643383279502884197e10 :: BigDecimal
      e = 3.141592653589793238462643383279502884197e-10 :: BigDecimal
      f = read "3.141592653589793238462643383279502884197e-10" :: BigDecimal

  print $ 100 * a
  print $ 100 * b
  print $ a == b
  print c
  print d
  print e
  print f
```

# BigFloating
in addition to the pretty complete BigDecimal module there is the rather scetchy BigFloating module.
BigFloating contains a few first steps to let `BigDecimal` instantiate the `Floating` typeclass.
As of now it contains arbitrary precision implementations for pi (based on Chudnovskis algorithm), sqrt and nthroot (based on Newtons classic algorithm).
All trigonometric functions, log and exp are still missing.
All code contributions are most welcome!

Here are some working examples:

```haskell
import           Data.BigDecimal
import           Data.BigFloating

-- | piChudnovsky computes pi using the Chudnovsky algorithm
bigPi :: BigDecimal
bigPi = piChudnovsky (DOWN, Just 100)

-- in western music theory a half tone step is the twelfth root of 2
halfToneStep :: BigDecimal
halfToneStep = nthRoot 2 12 (HALF_UP, Just 50)

-- take an input frequency and a number of half tone steps
-- and return the frequency of the note that is that many half tone steps
-- away from the input frequency
halfToneStepsFrom :: BigDecimal -> Integer -> BigDecimal
halfToneStepsFrom baseFrequency halfToneSteps =
  baseFrequency * halfToneStep ^^ halfToneSteps

main :: IO ()
main = do

  print bigPi

  print $ 2 / bigPi

  let r = 2

  print $ r ^ 2 * pi

  print $ sqrt 2

  print $ sqr 2 (halfUp 3000)

  print (pi :: BigFloating DOWN_100)
```

