{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Zodiac.Data.Symmetric(
    SymmetricAuthHeader(..)
  , parseSymmetricAuthHeader
  , renderSymmetricAuthHeader
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import qualified Data.Attoparsec.ByteString as AB
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

import           GHC.Generics (Generic)

import           P

import           Tinfoil.Data (HashFunction, renderHashFunction, parseHashFunction)

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
    } deriving (Eq, Show, Generic)

instance NFData SymmetricAuthHeader where rnf = genericRnf

renderSymmetricAuthHeader :: SymmetricAuthHeader -> ByteString
renderSymmetricAuthHeader (SymmetricAuthHeader TSRPv1 hf kid rts re sh) = BS.intercalate " " [
    renderSymmetricProtocol TSRPv1
  , T.encodeUtf8 (renderHashFunction hf)
  , renderKeyId kid
  , renderRequestTimestamp rts
  , renderRequestExpiry re
  , renderCSignedHeaders sh
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
  sh <- liftP parseCSignedHeaders =<< AB.takeByteString
  pure $ SymmetricAuthHeader proto hf kid rts re sh
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
