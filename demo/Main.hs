module Main
  ( main,
  )
where

import           Data.BigDecimal
import           Data.BigFloating
import           Data.Maybe       (fromJust)

bigPi :: BigDecimal
bigPi = piChudnovsky (DOWN, Just 100)

halfToneStep :: BigDecimal
halfToneStep = nthRoot 2 12 (HALF_UP, Just 50)

halfToneStepsFrom :: BigDecimal -> Integer -> BigDecimal
halfToneStepsFrom baseFrequency halfToneSteps =
  baseFrequency * halfToneStep ^^ halfToneSteps

rp :: String -> BigDecimal
rp str = read str :: BigDecimal

main :: IO ()
main = do
  let a = fromString "3.1415926"
      b = bigDecimal 31415926 7
      r = fromJust $ fromStringMaybe "2"
      c = 3.141592653589793238462643383279502884197 :: BigDecimal
      d = 3.141592653589793238462643383279502884197e20 :: BigDecimal
      e = 3.141592653589793238462643383279502884197e-20 :: BigDecimal
      f = read "3.141592653589793238462643383279502884197e-10" :: BigDecimal

  print $ 100 * a
  print $ 100 * b
  print $ a == b
  print c
  print d
  print e
  print f

  print bigPi

  print $ 2 / bigPi

  print $ r ^ 2 * pi

  print $ sqrt 2

  print $ sqr 2 (halfUp 3000)

  print $ roundBD (halfToneStepsFrom 440 12) (halfUp 2)

  print (pi :: BigFloating DOWN_100)
