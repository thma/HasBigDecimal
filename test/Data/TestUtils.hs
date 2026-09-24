{-# LANGUAGE CPP #-}
module Data.TestUtils where

import           Test.Hspec            hiding (it)
import qualified Test.Hspec as HS      (it)
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import           Test.QuickCheck       hiding (shrink)
import           Data.BigDecimal
#if !MIN_VERSION_QuickCheck(2,17,0)
import           Numeric.Natural
#endif

-- redefine it to use a sample with 1000 elements
it :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
it label action = modifyMaxSuccess (const 1000) $ HS.it label action

#if !MIN_VERSION_QuickCheck(2,17,0)
-- QuickCheck >= 2.17 provides Arbitrary Natural itself; defining it here as well
-- would be a duplicate instance. Keep our own only for older QuickCheck versions.
instance Arbitrary Natural where
  arbitrary = do
    NonNegative n <- arbitrary
    pure $ fromInteger n
#endif

-- arbitrary BigDecimals can be constructed using any Integer as unscaled value
-- and any non-negative Integer as scale
instance Arbitrary BigDecimal where
    arbitrary = do
      unscaledValue     <- arbitrary
      NonNegative scale <- arbitrary
      return $ bigDecimal unscaledValue scale
