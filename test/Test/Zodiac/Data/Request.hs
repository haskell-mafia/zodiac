{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.Data.Request where

import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO (IO)

import           Zodiac.Data.Request

import           Test.Zodiac.Arbitrary ()
import           Test.QuickCheck

prop_tripping_CMethod :: CMethod -> Property
prop_tripping_CMethod = tripping renderCMethod parseCMethod

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 100 } )
