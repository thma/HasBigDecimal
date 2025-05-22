{-# LANGUAGE OverloadedStrings #-}

module SpecHook where
-- | This module is a hook for Hspec to use the JUnit formatter.
-- | It is used to generate JUnit XML reports for the test suite.

import Test.Hspec
import Test.Hspec.JUnit.Config
import qualified Test.Hspec.JUnit.Formatter as Formatter

-- hook :: Spec -> Spec
-- hook = Formatter.use $ defaultJUnitConfig "test-suite"
hook :: Spec -> Spec
hook = Formatter.add $ defaultJUnitConfig "HasBigDecimal-test"

