{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Zodiac.Core.Data.Symmetric(
    SymmetricAuthHeader(..)
  , parseSymmetricAuthHeader
  , renderSymmetricAuthHeader
  , symmetricAuthHeaderEq
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import qualified Data.Attoparsec.ByteString as AB
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding as T

import           GHC.Generics (Generic)

import           P

import           System.IO (IO)

import           Tinfoil.Comparison (safeEq)
import           Tinfoil.Data (MAC(..))
import           Tinfoil.Encode (hexEncode)

import           Zodiac.Core.Data.Key
import           Zodiac.Core.Data.Protocol
import           Zodiac.Core.Data.Request
import           Zodiac.Core.Data.Time

data SymmetricAuthHeader =
  SymmetricAuthHeader {
      sahSymmetricProtocol :: !SymmetricProtocol
    , sahKeyId :: !KeyId
    , sahRequestTimestamp :: !RequestTimestamp
    , sahRequestExpiry :: !RequestExpiry
    , sahCSignedHeaders :: !CSignedHeaders
    , sahMAC :: !MAC
    } deriving (Show, Generic)

instance NFData SymmetricAuthHeader where rnf = genericRnf

-- | Compare in IO as this contains a MAC.
--
-- FIXME: need a better solution for this in tinfoil
symmetricAuthHeaderEq :: SymmetricAuthHeader -> SymmetricAuthHeader -> IO Bool
symmetricAuthHeaderEq (SymmetricAuthHeader sp1 kid1 rt1 re1 sh1 mac1) (SymmetricAuthHeader sp2 kid2 rt2 re2 sh2 mac2) = do
  mac <- (unMAC mac1) `safeEq` (unMAC mac2)
  pure $ and [
      sp1 == sp2
    , kid1 == kid2
    , rt1 == rt2
    , re1 == re2
    , sh1 == sh2
    , mac
    ]

renderSymmetricAuthHeader :: SymmetricAuthHeader -> ByteString
renderSymmetricAuthHeader (SymmetricAuthHeader TSRPv1 kid rts re sh mac) = BS.intercalate " " [
    renderSymmetricProtocol TSRPv1
  , renderKeyId kid
  , renderRequestTimestamp rts
  , renderRequestExpiry re
  , renderCSignedHeaders sh
  -- FIXME: do this in tinfoil
  , T.encodeUtf8 . hexEncode $ unMAC mac
  ]      

parseSymmetricAuthHeader :: ByteString -> Maybe' SymmetricAuthHeader
parseSymmetricAuthHeader bs = case AB.parseOnly symmetricAuthHeaderP bs of
  Right sah -> pure sah
  Left _ -> Nothing'

symmetricAuthHeaderP :: AB.Parser SymmetricAuthHeader
symmetricAuthHeaderP = do
  proto <- liftP parseSymmetricProtocol =<< next
  kid <- liftP parseKeyId =<< next
  rts <- liftP parseRequestTimestamp =<< next
  re <- liftP parseRequestExpiry =<< next
  sh <- liftP parseCSignedHeaders =<< next
  mac <- liftP parseMAC =<< AB.takeByteString
  pure $ SymmetricAuthHeader proto kid rts re sh mac
  where
    next = do
      p <- AB.takeTill isSpace
      AB.skip isSpace
      pure p

    isSpace 0x20 = True
    isSpace _ = False

    liftP f bs = case f bs of
      Just' x -> pure x
      Nothing' -> fail "Zodiac.Data.Symmetric.parseSymmetricAuthHeader"

-- FIXME: this belongs in tinfoil
parseMAC :: ByteString -> Maybe' MAC
parseMAC bs = case B16.decode bs of
  (x, "") -> if BS.length x == 32
               then Just' $ MAC x
               else Nothing'
  _ -> Nothing'
