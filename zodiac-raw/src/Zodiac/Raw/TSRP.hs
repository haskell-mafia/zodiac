{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.Raw.TSRP(
    authedRawRequest
  , httpAuthHeader
  , macHadronRequest
  ) where

import           Data.ByteString (ByteString)
import           Data.Semigroup ((<>))

import           Hadron (HTTPRequest(..))
import qualified Hadron as H

import           P hiding ((<>))

import           Tinfoil.Data (MAC, SymmetricKey)

import           Zodiac.Core.Data
import           Zodiac.Core.Request
import           Zodiac.Core.Symmetric
import           Zodiac.Raw.Error
import           Zodiac.Raw.Request

-- | Authenticate a raw HTTP request. If the request isn't
-- malformed, the output is a ByteString with the necessary
-- Authorization header added which can be sent directly to a server
-- supporting TSRP.
authedRawRequest :: KeyId
                 -> SymmetricKey
                 -> RequestExpiry
                 -> ByteString
                 -> RequestTimestamp
                 -> Either RequestError ByteString
authedRawRequest kid sk re bs rt = do
  r <- parseRawRequest bs
  cr <- fromHadronRequest r
  let mac = macRequest TSRPv1 kid rt re cr sk
      authH = httpAuthHeader TSRPv1 kid rt re cr mac
  pure . H.renderHTTPRequest $ addHeader r authH
  where
    addHeader (HTTPV1_1Request req) newH =
      let oldHs = H.unHTTPRequestHeaders $ H.hrqv1_1Headers req
          newHs = H.HTTPRequestHeaders $ (pure newH) <> oldHs in
      HTTPV1_1Request $ req { H.hrqv1_1Headers = newHs }

-- | Create a detached MAC of a hadron 'HTTPRequest'.
macHadronRequest :: KeyId
                 -> SymmetricKey
                 -> RequestExpiry
                 -> H.HTTPRequest
                 -> RequestTimestamp
                 -> Either RequestError MAC
macHadronRequest kid sk re r rts = do
  cr <- fromHadronRequest r
  pure $ macRequest TSRPv1 kid rts re cr sk

httpAuthHeader :: SymmetricProtocol
               -> KeyId
               -> RequestTimestamp
               -> RequestExpiry
               -> CRequest
               -> MAC
               -> H.Header
httpAuthHeader TSRPv1 kid rt re cr mac =
  let sh = signedHeaders cr
      sah = SymmetricAuthHeader TSRPv1 kid rt re sh mac in
  H.Header (H.HeaderName "authorization") . pure . H.HeaderValue $
    renderSymmetricAuthHeader sah
