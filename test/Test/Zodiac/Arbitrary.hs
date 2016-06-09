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
import           Tinfoil.Data.Hash (HashFunction(..))
import           Tinfoil.Data.Key (SymmetricKey(..))
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
    days <- ModifiedJulianDay <$> choose (0, 100000) -- uniform between 1878-11-17 and 2132-09-01
    secPart <- choose (0, 86401) -- seconds in a day (with possible leap second)
    pure . RequestTimestamp $ UTCTime days (secondsToDiffTime secPart)

instance Arbitrary RequestDate where
  arbitrary = timestampDate <$> arbitrary

instance Arbitrary KeyId where
  arbitrary = genUBytes KeyId 16

instance Arbitrary RequestExpiry where
  arbitrary = RequestExpiry <$> choose (1, maxBound)

-- FIXME: should use the instance in tinfoil
instance Arbitrary SymmetricKey where
  arbitrary = genUBytes SymmetricKey 32

instance Arbitrary HashFunction where
  arbitrary = elements [minBound..maxBound]

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
