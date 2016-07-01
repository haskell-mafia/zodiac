{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Zodiac.Core.Arbitrary where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (UTCTime(..), secondsToDiffTime)

import           Disorder.Core.Gen (utf8BS1, shrinkUtf8BS1)
import           Disorder.Corpus (muppets)

import           P

import           Zodiac.Core.Data

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Tinfoil.Arbitrary ()
import           Test.Zodiac.Core.Gen

instance Arbitrary CMethod where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary CPayload where
  arbitrary = do
    n <- choose (0, 1000)
    fmap (CPayload . BS.pack) . vectorOf n $ choose (0, 255)

instance Arbitrary CURI where
  arbitrary = do
    ps <- listOf (elements muppets)
    pure . CURI . T.encodeUtf8 . ("/" <>) $ T.intercalate "/" ps

-- I don't think this needs to be particularly realistic.
instance Arbitrary CQueryString where
  arbitrary = encodeCQueryString <$> utf8BS1

  shrink = fmap encodeCQueryString . shrinkUtf8BS1 . unCQueryString

instance Arbitrary CHeaderName where
  arbitrary = do
    n <- choose (1, 100)
    v <- vectorOf n (elements [0x61..0x7a]) -- alpha
    pure . CHeaderName . T.decodeUtf8 $ BS.pack v

instance Arbitrary CSignedHeaders where
  arbitrary = CSignedHeaders <$> arbitrary

instance Arbitrary CHeaderValue where
  arbitrary =
    (CHeaderValue . T.encodeUtf8) <$> (elements muppets)

instance Arbitrary CHeaders where
  arbitrary = do
    hs <- listOf arbitrary
    host <- (,) (CHeaderName "host") <$> arbitrary
    pure . CHeaders . M.fromList $ host : hs

instance Arbitrary CRequest where
  arbitrary = CRequest
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary SymmetricProtocol where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary AsymmetricProtocol where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Protocol where
  arbitrary = oneof [Symmetric <$> arbitrary, Asymmetric <$> arbitrary]

-- Generate timestamps to second precision, which is all we care about/support.
instance Arbitrary RequestTimestamp where
  arbitrary = do
    -- Reasonable mix of sensible dates and ridiculous ones.
    days <- oneof [
        sensible
      , whenIWasALad
      , hurdReleaseDate
      ]
    -- Seconds in a day (with possible leap second).
    secPart <- choose (0, 86401)
    pure . RequestTimestamp $ UTCTime days (secondsToDiffTime secPart)
    where
      -- Uniform between 1858-11-17 and 4616-10-14.
      sensible = ModifiedJulianDay <$> choose (0, 100000)

      -- Uniform between 0001-01-01 and 1858-11-17
      whenIWasALad = ModifiedJulianDay <$> choose ((- 678575), 0)

      -- Uniform between 4616-10-14 and 2739765-11-19.
      hurdReleaseDate = ModifiedJulianDay <$> choose (100000, 100000000)

instance Arbitrary RequestDate where
  arbitrary = timestampDate <$> arbitrary

instance Arbitrary KeyId where
  arbitrary = genUBytes KeyId 16

instance Arbitrary RequestExpiry where
  -- Uniform over valid expiry values.
  arbitrary = RequestExpiry <$> choose (1, maxRequestExpiry)

instance Arbitrary SymmetricAuthHeader where
  arbitrary = SymmetricAuthHeader <$> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
