{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.Symmetric(
    authenticationString
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

import           P

import           Tinfoil.Data (HashFunction, renderHashFunction, renderHash)
import           Tinfoil.Hash (hash)

import           Zodiac.Data
import           Zodiac.Request

-- | Construct a string to authenticate, from all the information which will
-- be included in the request MAC.
authenticationString :: SymmetricProtocol
                     -> HashFunction
                     -> KeyId
                     -> RequestTimestamp
                     -> RequestExpiry
                     -> CRequest
                     -> ByteString
authenticationString TSRPv1 hf kid rts re cr =
  let hcr = hash hf $ renderCRequest cr in
  BS.intercalate "\n" [
      renderSymmetricProtocol TSRPv1
    , T.encodeUtf8 (renderHashFunction hf)
    , renderKeyId kid
    , renderRequestTimestamp rts
    , renderRequestExpiry re
    , T.encodeUtf8 (renderHash hcr)
    ]
