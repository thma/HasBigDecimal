module Main
  ( main,
  )
where

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

  -- 12 half tone steps is an octave, so the resulting frequency should be 880
  print $ roundBD (halfToneStepsFrom 440 12) (halfUp 2)


 -- BigFloating operations
  print bigPi

  print $ 2 / bigPi

  let r = 10

  print $ r ^ 2 * pi

  print $ sqrt 2

  print $ sqr 2 (halfUp 3000)

  print (pi :: BigFloating DOWN_100)
