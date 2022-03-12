module Main where

import Data.BigDecimal

bd :: String -> BigDecimal 
bd = read

main :: IO ()
main = do
  let a = bd "3.1415926"
      b = bigDecimal 31415926 7
  print $ 100 * a
  print $ 100 * b
  print $ a == b