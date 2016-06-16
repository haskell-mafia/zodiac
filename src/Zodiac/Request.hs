{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.Request(
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

import           Tinfoil (hexEncode, unHash)
import           Tinfoil.Hash (hashSHA256)

import           Zodiac.Data.Request

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
