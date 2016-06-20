{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Zodiac.Gen where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (UTCTime(..), addUTCTime, secondsToDiffTime)

import           P

import           System.Random (Random)

import           Test.QuickCheck

import           Zodiac.Core.Data

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

genInvalidExpiry :: Gen ByteString
genInvalidExpiry =
  fmap (T.encodeUtf8 . renderIntegral) $ oneof [tooSmall, tooBig]
  where
    tooSmall = choose (- maxBound, 0)

    tooBig = choose (maxRequestExpiry, maxBound)

-- | Negative in a calendrical sense (i.e., before 0001-01-01).
genNegativeTimestamp :: Gen ByteString
genNegativeTimestamp = do
  days <- ModifiedJulianDay <$> choose (- (2 ^ (256 :: Integer)), - 678575)
  secPart <- choose (0, 86401)
  pure . renderRequestTimestamp . RequestTimestamp $
    UTCTime days (secondsToDiffTime secPart)
