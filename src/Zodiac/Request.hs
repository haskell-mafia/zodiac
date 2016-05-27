{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.Request(
    toCanonicalRequest
  ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Network.HTTP.Client (Request, RequestBody(..))
import qualified Network.HTTP.Client as HC

import           P hiding ((<>))

import           Zodiac.Data.Error
import           Zodiac.Data.Request

-- | Convert an http-client Request to canonical form for signing or
-- verification.
toCanonicalRequest :: Request -> Either RequestError CRequest
toCanonicalRequest r =
  let uri = CURI $ HC.path r
      qs = CQueryString $ HC.queryString r in do
  method <- reqCMethod r
  payload <- reqCPayload r
  headers <- reqCHeaders r
  pure $ CRequest method uri qs headers payload

-- | This will not include Content-Length or Transfer-Encoding as these are
-- set automatically by http-client. If at some point these need to be included
-- in the signature, they can be set here explicitly and http-client will not
-- override them.
reqCHeaders :: Request -> Either RequestError (Map CHeaderName (NonEmpty CHeaderValue))
reqCHeaders r =
  case HC.requestHeaders r of
    [] -> Left NoRequestHeaders
    hs -> foldM updateHeaderMap M.empty hs
  where
    updateHeaderMap acc (hn, hv) = do
      hn' <- fmap (CHeaderName . T.toLower) .
        first (const (HeaderNameInvalidUTF8 $ CI.original hn)) . T.decodeUtf8' $ CI.original hn
      pure $ M.insertWith (<>) hn' (pure $ CHeaderValue hv) acc

reqCMethod :: Request -> Either RequestError CMethod
reqCMethod r =
  case parseCMethod (HC.method r) of
    Just' m -> pure m
    Nothing' -> Left . InvalidHTTPMethod $ HC.method r

reqCPayload :: Request -> Either RequestError CPayload
reqCPayload r = do
  bs <- case HC.requestBody r of
    RequestBodyLBS b -> pure $ BSL.toStrict b
    RequestBodyBS b -> pure b
    _ -> Left UnsupportedPayloadType
  pure $ CPayload bs
