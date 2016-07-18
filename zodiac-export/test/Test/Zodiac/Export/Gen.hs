{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Zodiac.Export.Gen where

import           Foreign.C.Types (CTime(..))

import           P

import           Test.QuickCheck

genCTime = CTime <$> choose (minBound, maxBound)
