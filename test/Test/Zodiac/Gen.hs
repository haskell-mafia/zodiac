{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Zodiac.Gen where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Time.Clock (UTCTime(..), addUTCTime)

import           P

import           System.Random (Random)

import           Test.QuickCheck

import           Zodiac.Data

-- Generate bytes uniformly.
genUBytes :: (ByteString -> a) -> Int -> Gen a
genUBytes f n =
  fmap (f . BS.pack) . vectorOf n $ choose (0, 255)

genNPlus :: (Integral a, Arbitrary a, Bounded a, Random a) => Gen a
genNPlus = choose (1, maxBound)

genTimeWithin :: RequestTimestamp -> RequestExpiry -> Gen UTCTime
genTimeWithin (RequestTimestamp rt) (RequestExpiry re) = do
  secs <- choose (0, re - 1)
  pure $ addUTCTime (fromIntegral secs) rt

genTimeBefore :: RequestTimestamp -> Gen UTCTime
genTimeBefore (RequestTimestamp rt) = do
  secs <- choose (1, 1000) :: Gen Int
  pure $ addUTCTime (fromIntegral (- secs)) rt

genTimeExpired :: RequestTimestamp -> RequestExpiry -> Gen UTCTime
genTimeExpired (RequestTimestamp rt) (RequestExpiry re) = do
  secs <- choose (re, maxBound)
  pure $ addUTCTime (fromIntegral secs) rt


