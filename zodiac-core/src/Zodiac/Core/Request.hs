{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.Core.Request(
    renderCRequest
  , signedHeaders
  , stripCRequest
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text.Encoding as T

import           P

import           Tinfoil.Data.Hash (unHash)
import           Tinfoil.Encode (hexEncode)
import           Tinfoil.Hash (hashSHA256)

import           Zodiac.Core.Data.Request

-- | Get the list of names of headers we will sign (which is all of the headers
-- in the request we're given). Any unsigned headers will either be our
-- authentication header or will have been added by proxies.
signedHeaders :: CRequest -> CSignedHeaders
signedHeaders cr =
  let hns = NE.fromList . M.keys . unCHeaders $ crHeaders cr in
  CSignedHeaders hns

-- | Return a version of the canonical request with all headers which
-- don't appear in the list of signed headers removed.
--
-- We don't bother verifying that all of the signed headers claimed
-- here are actually present as missing headers will cause the
-- request hashes to differ.
--
-- This will (for a well-formed request) also strip the auth header we added
-- during signing.
stripCRequest :: CRequest -> CSignedHeaders -> StrippedCRequest
stripCRequest cr (CSignedHeaders shs) =
  let oldHeaders = unCHeaders $ crHeaders cr
      newHeaders = CHeaders $ M.filterWithKey isSigned oldHeaders in
  StrippedCRequest $ cr { crHeaders = newHeaders }
  where
    isSigned hn _ = elem hn shs

renderCRequest :: CRequest -> ByteString
renderCRequest (CRequest m u qs hs p) =
  let pHash = T.encodeUtf8 . hexEncode . unHash . hashSHA256 $ unCPayload p in
  BS.intercalate "\n" [
      renderCMethod m
    , renderCURI u
    , renderCQueryString qs
    , renderCHeaders hs
    , pHash
    ]
