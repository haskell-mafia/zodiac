{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Convert raw HTTP request ByteStrings into canonical requests.
-}
module Zodiac.Raw.Request (
    toCanonicalRequest
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import           Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import           Data.Semigroup ((<>))
import qualified Data.Text.Encoding as T

import           Hadron (HTTPRequest(..), HTTPRequestV1_1(..), Header(..))
import           Hadron (HTTPRequestHeaders(..), RequestTarget(..))
import           Hadron (Fragment(..), QueryString(..))
import           Hadron (parseHTTPRequest)
import qualified Hadron as H

import           P hiding ((<>))

import           Zodiac.Core.Data.Request
import qualified Zodiac.Core.Header as Z
import           Zodiac.Raw.Error

import           X.Data.ByteString.Char8 (asciiToLower)

toCanonicalRequest :: ByteString -> Either RequestError CRequest
toCanonicalRequest bs =
  (first HadronError $ parseHTTPRequest bs) >>= fromHadronRequest

fromHadronRequest :: HTTPRequest -> Either RequestError CRequest
fromHadronRequest (HTTPV1_1Request r) =
  let bsMethod = H.renderHTTPMethod $ hrqv1_1Method r
      cbody = CPayload . H.renderRequestBody $ hrqv1_1Body r
      cqs = hQueryString $ hrqv1_1Target r
      curi = hURI $ hrqv1_1Target r in do
  cm <- maybeRequest (InvalidHTTPMethod bsMethod) $ parseCMethod bsMethod
  chs <- fromHadronHeaders $ hrqv1_1Headers r
  pure $ CRequest cm curi cqs chs cbody
  where
    maybeRequest _ (Just' x) = pure x
    maybeRequest e Nothing' = Left e

    -- The "query string part" in the TSRP spec includes the URI fragment.
    hQueryString (AbsPathTarget _uri qs frag) =
      -- https://github.com/ambiata/hadron/issues/12
      let fragPart = if nullFragment frag then "" else H.renderFragment frag in
      CQueryString $ (canonicalQS qs) <> fragPart

    hURI (AbsPathTarget uri _qs _frag) =
      CURI $ H.renderURIPath uri

    nullFragment NoFragment = True
    nullFragment (FragmentPart _) = False

    canonicalQS NoQueryString = ""
    canonicalQS (QueryStringPart x) = x

toHadronRequest :: CRequest -> HTTPRequest
toHadronRequest (CRequest m u qs hs p) =
  let hmethod = HTTPMethod $ renderCMethod m
      htarget = toTarget u qs
      hhs = toHadronHeaders hs
      hbody = RequestBody $ unCPayload p in
  HTTPV1_1Request $ HTTPRequestV1_1 hmethod htarget hhs hbody
  where
    toTarget (CURI cu) (CQueryString cqs) =
      let tURI = H.URIPath cu
          -- Break on # to split into query string and fragment.
          (pre, suff) = BS.span (/= 0x23) cqs
          tFrag = if BS.null suff
                    then H.NoFragment
                    else H.FragmentPart $ BS.tail suff
          tQS = if BS.null pre
                  then H.NoQueryString
                  else H.QueryStringPart pre in
       AbsPathTarget tURI tQS tFrag

    toHadronHeaders (CHeaders hmap) =
      let xs = fmap (bimap (T.encodeUtf8 . unCHeaderName) (fmap unCHeaderValue)) $ M.assocs hmap
          xs' = fmap (bimap H.HeaderName (fmap H.HeaderValue)) xs in
      HTTPRequestHeaders . fmap (uncurry H.Header) $ NE.fromList xs'

fromHadronHeaders :: HTTPRequestHeaders -> Either RequestError CHeaders
fromHadronHeaders (HTTPRequestHeaders hs) =
  fmap CHeaders $ F.foldlM updateHeaderMap M.empty hs
  where
    updateHeaderMap acc (Header hn hvs) = do
      -- Error can't happen, hadron validates the encoding.
      hn' <- first (const (HeaderNameInvalidUTF8 $ H.renderHeaderName hn)) .
        T.decodeUtf8' . asciiToLower $ H.renderHeaderName hn
      pure $ M.insertWith (<>) (CHeaderName hn') (renderValues hvs) acc

    renderValues hvs = join $ fmap (renderValue . H.unHeaderValue) hvs

    renderValue hv =
      -- Split on comma.
      let hvs = fmap (CHeaderValue . Z.trimSpaces) $ BS.split 0x2c hv in
      case nonEmpty hvs of
        Nothing -> pure $ CHeaderValue ""
        Just xs -> xs
