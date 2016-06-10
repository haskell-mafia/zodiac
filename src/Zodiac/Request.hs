{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.Request(
    renderCRequest
  , signedHeaders
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
