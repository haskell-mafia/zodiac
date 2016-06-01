{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Zodiac.Arbitrary where

import           P

import           Zodiac.Data.Request

import           Test.QuickCheck (Arbitrary(..), elements)

instance Arbitrary CMethod where
  arbitrary = elements [minBound..maxBound]

