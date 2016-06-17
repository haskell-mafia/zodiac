{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Zodiac.Arbitrary where

import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (UTCTime(..), secondsToDiffTime)

import           Disorder.Core.Gen (utf8BS1)
import           Disorder.Corpus (muppets)

import           P

import           Zodiac.Data

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Zodiac.Gen

-- FIXME: expose these instances from tinfoil
import           Tinfoil.Data.Key (SymmetricKey(..))
import           Tinfoil.Data.MAC (MAC(..))
import           Tinfoil.Encode (hexEncode)

-- FIXME: should find a better home for this instance at some point
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = NE.fromList <$> listOf1 arbitrary

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
  arbitrary =
    encodeCQueryString <$> utf8BS1

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
  arbitrary =
    fmap (CHeaders . M.fromList) $ listOf1 arbitrary

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
      -- Uniform between 0001-01-01 and 4616-10-14.
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

-- FIXME: should use the instance in tinfoil
instance Arbitrary SymmetricKey where
  arbitrary = genUBytes SymmetricKey 32

instance Arbitrary MAC where
  arbitrary = genUBytes MAC 32

-- testing only
instance Show SymmetricKey where
  show = T.unpack . hexEncode . unSymmetricKey

instance Arbitrary SymmetricAuthHeader where
  arbitrary = SymmetricAuthHeader <$> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
