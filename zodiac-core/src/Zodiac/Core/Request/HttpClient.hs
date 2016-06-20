{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Convert http-client Request objects into canonical requests.
-}
module Zodiac.Core.Request.HttpClient(
    fromCanonicalRequest
  , toCanonicalRequest
  , trimSpaces
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import           Data.Default.Class (def)
import           Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Network.HTTP.Client (Request, RequestBody(..))
import qualified Network.HTTP.Client as HC

import           P hiding ((<>))

import           Zodiac.Core.Data.Error
import           Zodiac.Core.Data.Request

-- | Convert an http-client Request to canonical form for signing or
-- verification.
toCanonicalRequest :: Request -> Either RequestError CRequest
toCanonicalRequest r =
  -- URI part and query string are both already URL-encoded in http-client.
  let uri = CURI $ HC.path r
      qs = CQueryString $ HC.queryString r in do
  method <- reqCMethod r
  payload <- reqCPayload r
  headers <- reqCHeaders r
  pure $ CRequest method uri qs headers payload

fromCanonicalRequest :: CRequest -> Request
fromCanonicalRequest (CRequest m u qs hs p) =
  def {
    HC.method = renderCMethod m
  , HC.path = unCURI u
  , HC.queryString = unCQueryString qs
  , HC.requestHeaders = hs'
  , HC.requestBody = HC.RequestBodyBS (unCPayload p)
  }
  where
    hs' =
      M.elems . M.mapWithKey renderHeader $ unCHeaders hs

    renderHeader (CHeaderName hn) vs =
      let hn' = CI.mk $ T.encodeUtf8 hn
          hvs = BS.intercalate "," . NE.toList $ unCHeaderValue <$> vs in
      (hn', hvs)

-- | This will not include Content-Length or Transfer-Encoding as these are
-- set automatically by http-client. If at some point these need to be included
-- in the signature, they can be set here explicitly and http-client will not
-- override them.
reqCHeaders :: Request -> Either RequestError CHeaders
reqCHeaders r =
  case HC.requestHeaders r of
    [] -> Left NoRequestHeaders
    hs -> fmap CHeaders $ foldM updateHeaderMap M.empty hs
  where
    updateHeaderMap acc (hn, hv) = do
      hn' <- fmap (CHeaderName . T.toLower) .
        first (const (HeaderNameInvalidUTF8 $ CI.original hn)) . T.decodeUtf8' $ CI.original hn
      pure $ M.insertWith (<>) hn' (renderValues hv) acc

    renderValues hv =
      -- split on comma
      let hvs = fmap (CHeaderValue . trimSpaces) $ BS.split 0x2c hv in
      case nonEmpty hvs of
        -- Empty header value -> singleton empty header value.
        Nothing -> pure $ CHeaderValue ""
        -- One or more -> trim and return comma-separated values.
        Just xs -> xs

-- | Remove leading and trailing spaces, replace all internal strings
-- of spaces with a single space.
trimSpaces :: ByteString -> ByteString
trimSpaces x =
  let gps = foldSpace <$> BS.group x in
  trimTrailing . trimLeading $ BS.concat gps
  where
    trimTrailing bs = case BS.unsnoc bs of
      Nothing -> ""
      Just (xs, 0x20) -> xs
      Just (xs, y) -> BS.snoc xs y

    trimLeading bs = case BS.uncons bs of
      Nothing -> ""
      Just (0x20, xs) -> xs
      Just (y, xs) -> BS.cons y xs

    foldSpace bs = case BS.head bs of
      0x20 -> BS.singleton 0x20
      _ -> bs

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

