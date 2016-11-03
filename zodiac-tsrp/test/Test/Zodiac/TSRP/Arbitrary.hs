{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Zodiac.TSRP.Arbitrary where

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Tinfoil.Arbitrary ()
import           Test.Zodiac.Core.Arbitrary ()
import           Test.Zodiac.Core.Gen

import           Zodiac.TSRP.Data

instance Arbitrary KeyId where
  arbitrary = genUBytes KeyId 16

instance Arbitrary SymmetricAuthHeader where
  arbitrary = SymmetricAuthHeader <$> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary

instance Arbitrary TSRPKey where
  arbitrary = TSRPKey <$> arbitrary
