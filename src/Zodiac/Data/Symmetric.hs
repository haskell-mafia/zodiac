{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Zodiac.Data.Symmetric(
    SymmetricAuthHeader(..)
  , renderSymmetricAuthHeader
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

import           GHC.Generics (Generic)

import           P

import           Tinfoil.Data (HashFunction, renderHashFunction)

import           Zodiac.Data.Key
import           Zodiac.Data.Protocol
import           Zodiac.Data.Request

data SymmetricAuthHeader =
  SymmetricAuthHeader {
      sahSymmetricProtol :: !SymmetricProtocol
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

