{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Bench.Zodiac.Core.MAC
import qualified Bench.Zodiac.Core.Symmetric

import           Criterion.Main
import           Criterion.Types

import           P

import           System.IO

import           Test.Zodiac.Core.Arbitrary ()

zodiacBench :: [Benchmark] -> IO ()
zodiacBench = defaultMainWith cfg
  where
    cfg = defaultConfig {
            reportFile = Just "dist/build/zodiac-bench.html"
          , csvFile = Just "dist/build/zodiac-bench.csv"
          }

main :: IO ()
main = zodiacBench $ join [
    Bench.Zodiac.Core.MAC.benchmarks
  , Bench.Zodiac.Core.Symmetric.benchmarks
  ]
