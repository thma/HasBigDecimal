module Main where

import Data.BigDecimal

main = do
  let a = read "3.1415926" :: BigDecimal
      b = bigDecimal 31415926 7
  print $ 100 * a
  print $ 100 * b
  print $ a == b