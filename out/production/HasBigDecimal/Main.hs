module Main where

import Data.BigDecimal
import Text.Read          (readEither)

main :: IO ()
main = do
    input <- getLine
    case readEither input of
      Left ex -> putStrLn $ show ex
      Right p -> putStrLn $ toString $ piChudnovsky (DOWN, Just p)
