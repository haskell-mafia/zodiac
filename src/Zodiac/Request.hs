{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.Request(
    renderCRequest
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

import           P

import           Tinfoil (hexEncode, unHash)
import           Tinfoil.Hash (hashSHA256)

import           Zodiac.Data.Request

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
