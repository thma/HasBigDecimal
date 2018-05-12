module Main where

import Data.BigDecimal
import Data.BigFloating
import Control.Exception

main :: IO ()
main = do
  input <- try readLn :: IO (Either SomeException Integer)
  case input of
    Left  _ -> putStrLn "ERROR: please provide an integer to specify how many decimals of pi you want me to compute"
    Right p -> putStrLn $ toString $ piChudnovsky (DOWN, Just $ abs p)
