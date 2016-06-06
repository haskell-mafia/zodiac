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

import           Disorder.Core.Gen (utf8BS1)
import           Disorder.Corpus (muppets)

import           P

import           Zodiac.Data

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Zodiac.Gen

import           Tinfoil.Data

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

instance Arbitrary Protocol where
  arbitrary = elements [minBound..maxBound]

-- quickcheck-instances (day + seconds) - days clustered in 19th century,
-- seconds in a day are uniform.
instance Arbitrary RequestTimestamp where
  arbitrary = RequestTimestamp <$> arbitrary

-- FIXME: should use the instance in tinfoil
instance Arbitrary KeyId where
  arbitrary = genUBytes KeyId 32

-- FIXME: should use the instance in tinfoil
instance Arbitrary SymmetricKey where
  arbitrary = genUBytes SymmetricKey 32

