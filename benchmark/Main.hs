module Main (
  main
) where

import Criterion.Main (bench, defaultMain, nf)
import Data.BigDecimal hiding (nf, precision)
import Numeric.Natural (Natural)
import Data.Foldable (foldl')
--import Data.Text as T hiding (foldl')

main :: IO ()
main = benchmarks

precision :: BigDecimal -> Natural
precision 0 = 1
precision bd = go 1 $ abs val
  where
    val     = value bd
    go ds n = if n >= 10 then go (ds + 1) (n `div` 10) else ds

precision' :: BigDecimal -> Natural
precision' = len . show . abs . value
  where
    len :: [a] -> Natural
    len = foldl' (\c _ -> c+1) 0

-- using Data.Text is slightly faster than String for very large numbers.
-- But IMHO it's not worth to add a dependency to text just for these edge cases 
-- precision'' :: BigDecimal -> Natural
-- precision'' = fromInteger . fromIntegral . T.length . T.pack . show . abs . value


benchmarks :: IO ()
benchmarks = do
  let bigNum = fromInteger (3 * 10 ^ 100000)

  defaultMain
    [ bench "precision using division" $ nf precision bigNum
    ,  bench "precision using show" $ nf precision' bigNum
--    ,  bench "precision using Text" $ nf precision'' bigNum
    ]
  return ()
