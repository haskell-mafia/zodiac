{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
module Bench.Zodiac where

import           Disorder.Core.Gen (GenSeed(..), genDeterministic)

import           P

import           System.IO (IO)

import           Test.QuickCheck (Gen)

generate' :: Gen a -> IO a
generate' = pure . genDeterministic (GenSeed 2718281)

