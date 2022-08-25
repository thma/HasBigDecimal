module Main where

import Criterion.Main ( defaultMain, bench, nf )
import Data.BigDecimal hiding (precision, nf)
import GHC.Natural (Natural)

main :: IO ()
main = benchmarks

precision :: BigDecimal -> Natural
precision 0                  = 1
precision (BigDecimal val _) = go 1 $ abs val
  where go ds n = if n >= 10 then go (ds + 1) (n `div` 10) else ds

precision' :: BigDecimal -> Natural
precision' 0                  = 1
precision' (BigDecimal val _) = (fromInteger . toInteger . length . show . abs) val


benchmarks :: IO ()
benchmarks = do
  let bigNum = fromInteger (3 * 10 ^ 1000)  

  defaultMain [
        bench "precision using division"  $ nf precision  bigNum
      , bench "precision using show"      $ nf precision' bigNum
      ]
  return ()
