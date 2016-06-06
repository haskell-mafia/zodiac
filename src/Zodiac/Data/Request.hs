{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Zodiac.Data.Request(
    CHeaderName(..)
  , CHeaders(..)
  , CHeaderValue(..)
  , CMethod(..)
  , CQueryString(..)
  , CPayload(..)
  , CRequest(..)
  , CURI(..)
  , RequestExpiry(..)
  , RequestTimestamp(..)
  , encodeCURI
  , encodeCQueryString
  , parseCMethod
  , renderCHeaders
  , renderCMethod
  , renderCQueryString
  , renderCURI
  , renderTimestampDate
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)

import           Network.HTTP.Types.URI (urlEncode)

import           GHC.Generics (Generic)

import           P

-- | One of the methods explicitly defined in section 9 of RFC 2616, we don't
-- allow any others.
data CMethod =
    OPTIONS
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | TRACE
  deriving (Eq, Show, Generic, Enum, Bounded)

instance NFData CMethod where rnf = genericRnf

renderCMethod :: CMethod -> ByteString
renderCMethod OPTIONS = "OPTIONS"
renderCMethod GET = "GET"
renderCMethod HEAD = "HEAD"
renderCMethod POST = "POST"
renderCMethod PUT = "PUT"
renderCMethod DELETE = "DELETE"
renderCMethod TRACE = "TRACE"

parseCMethod :: ByteString -> Maybe' CMethod
parseCMethod "OPTIONS" = Just' OPTIONS
parseCMethod "GET" = Just' GET
parseCMethod "HEAD" = Just' HEAD
parseCMethod "POST" = Just' POST
parseCMethod "PUT" = Just' PUT
parseCMethod "DELETE" = Just' DELETE
parseCMethod "TRACE" = Just' TRACE
parseCMethod _ = Nothing'

-- | The bit from the last character of the host part of the URL to either
-- the ? beginning the query string or the end of the URL.
--
-- Represented as a *URL-encoded* ByteString.
newtype CURI =
  CURI {
    unCURI :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData CURI where rnf = genericRnf

renderCURI :: CURI -> ByteString
renderCURI = unCURI

-- | Path elements are URL-encoded with ' ' encoded as '%20'.
encodeCURI :: ByteString -> CURI
encodeCURI = CURI . urlEncode False -- don't encode ' ' as '+'

-- | From the ? (not inclusive) to the end of the URL.
--
-- Represented as a *URL-encoded* ByteString.
newtype CQueryString =
  CQueryString {
    unCQueryString :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData CQueryString where rnf = genericRnf

renderCQueryString :: CQueryString -> ByteString
renderCQueryString = unCQueryString

-- | Query strings are URL-encoded with ' ' encoded as '+'.
encodeCQueryString :: ByteString -> CQueryString
encodeCQueryString = CQueryString . urlEncode True -- encode ' ' as '+'

-- | Header name. The canonical form is lowercase, but that's done on render.
newtype CHeaderName =
  CHeaderName {
    unCHeaderName :: Text
  } deriving (Eq, Show, Generic, Ord)

instance NFData CHeaderName where rnf = genericRnf

newtype CHeaderValue =
  CHeaderValue {
    unCHeaderValue :: ByteString
  } deriving (Eq, Show, Generic, Ord)

instance NFData CHeaderValue where rnf = genericRnf

newtype CHeaders =
  CHeaders {
    unCHeaders :: Map CHeaderName (NonEmpty CHeaderValue)
  } deriving (Eq, Show, Generic)

instance NFData CHeaders where rnf = genericRnf

renderCHeaders :: CHeaders -> ByteString
renderCHeaders (CHeaders hs) =
  let hassocs = sortOn fst $ M.assocs hs
      hs' = (uncurry renderHeader) <$> hassocs in
  BS.intercalate "\n" hs'
  where
    renderHeader hn hvs =
      let hn' = T.encodeUtf8 . T.toLower $ unCHeaderName hn in
      BS.concat $ [hn', ":", BS.intercalate "," (NE.toList $ unCHeaderValue <$> hvs)]

-- | Body of request.
newtype CPayload =
  CPayload {
    unCPayload :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData CPayload where rnf = genericRnf

-- | A canonical request for signing. Contains all data needed to generate
-- a ByteString which can be signed or verified.
data CRequest =
  CRequest {
      crMethod :: !CMethod
    , crURI :: !CURI
    , crQueryString :: !CQueryString
    -- | HOST header always required.
    , crHeaders :: !CHeaders
    , crPayload :: !CPayload
    } deriving (Show, Generic)

instance NFData CRequest where rnf = genericRnf

instance Eq CRequest where
  (CRequest m1 uri1 qs1 hs1 p1) == (CRequest m2 uri2 qs2 hs2 p2) =
    and [
        m1 == m2
      , uri1 == uri2
      , qs1 == qs2
      , hs1 `headerEq` hs2
      , p1 == p2
      ]
    where
      headerEq (CHeaders h1) (CHeaders h2) =
        let ks1 = M.keys h1
            ks2 = M.keys h2
            vs1 = L.sort (NE.sort <$> M.elems h1)
            vs2 = L.sort (NE.sort <$> M.elems h2) in
        and [
            ks1 == ks2
          , vs1 == vs2
          ]

-- | Time at which the request is made.
newtype RequestTimestamp =
  RequestTimestamp {
    unRequestTimestamp :: UTCTime
  } deriving (Eq, Generic)

-- | Render just the date part in ISO-8601 format.
renderTimestampDate :: RequestTimestamp -> ByteString
renderTimestampDate (RequestTimestamp ts) =
  let fmt = iso8601DateFormat Nothing
      str = formatTime defaultTimeLocale fmt ts in
  BSC.pack str

instance NFData RequestTimestamp where rnf = genericRnf

-- | Number of seconds for a request to be considered valid - after
-- this time, an application server will discard it.
newtype RequestExpiry =
  RequestExpiry {
    unRequestExpiry :: Int
  } deriving (Eq, Show, Generic)

instance NFData RequestExpiry where rnf = genericRnf
