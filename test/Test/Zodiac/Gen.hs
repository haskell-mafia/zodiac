{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Zodiac.Gen where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P

import           Test.QuickCheck

-- Generate bytes uniformly.
genUBytes :: (ByteString -> a) -> Int -> Gen a
genUBytes f n =
  fmap (f . BS.pack) . vectorOf n $ choose (0, 255)
