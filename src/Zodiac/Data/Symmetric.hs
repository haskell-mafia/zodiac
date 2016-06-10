{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Zodiac.Data.Symmetric(
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
import           Tinfoil.Data (HashFunction, MAC(..))
import           Tinfoil.Data (renderHashFunction, parseHashFunction)
import           Tinfoil.Encode (hexEncode)

import           Zodiac.Data.Key
import           Zodiac.Data.Protocol
import           Zodiac.Data.Request
import           Zodiac.Data.Time

data SymmetricAuthHeader =
  SymmetricAuthHeader {
      sahSymmetricProtocol :: !SymmetricProtocol
    , sahHashFunction :: !HashFunction
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
symmetricAuthHeaderEq (SymmetricAuthHeader sp1 hf1 kid1 rt1 re1 sh1 mac1) (SymmetricAuthHeader sp2 hf2 kid2 rt2 re2 sh2 mac2) = do
  mac <- (unMAC mac1) `safeEq` (unMAC mac2)
  pure $ and [
      sp1 == sp2
    , hf1 == hf2
    , kid1 == kid2
    , rt1 == rt2
    , re1 == re2
    , sh1 == sh2
    , mac
    ]

renderSymmetricAuthHeader :: SymmetricAuthHeader -> ByteString
renderSymmetricAuthHeader (SymmetricAuthHeader TSRPv1 hf kid rts re sh mac) = BS.intercalate " " [
    renderSymmetricProtocol TSRPv1
  , T.encodeUtf8 (renderHashFunction hf)
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
  hf <- utf8P (liftP parseHashFunction) =<< next
  kid <- liftP parseKeyId =<< next
  rts <- liftP parseRequestTimestamp =<< next
  re <- liftP parseRequestExpiry =<< next
  sh <- liftP parseCSignedHeaders =<< next
  mac <- liftP parseMAC =<< AB.takeByteString
  pure $ SymmetricAuthHeader proto hf kid rts re sh mac
  where
    utf8P p bs = case T.decodeUtf8' bs of
      Left e -> fail $ "error decoding UTF-8: " <> show e
      Right t -> p t

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
