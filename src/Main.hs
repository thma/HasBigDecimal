module Main where

import Data.BigDecimal

main :: IO ()
main = do
    precision <- readLn :: IO Integer
    putStrLn $ toString $ piChudnovsky (DOWN, Just precision)
