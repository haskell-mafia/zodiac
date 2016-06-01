{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Zodiac.Data.Request(
    CHeaderName(..)
  , CHeaderValue(..)
  , CMethod(..)
  , CQueryString(..)
  , CPayload(..)
  , CRequest(..)
  , CURI(..)
  , parseCMethod
  , renderCMethod
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Map.Strict (Map)

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
newtype CURI =
  CURI {
    unCURI :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData CURI where rnf = genericRnf

-- | From the ? (not inclusive) to the end of the URL.
newtype CQueryString =
  CQueryString {
    unCQueryString :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData CQueryString where rnf = genericRnf

-- | Header name. The canonical form is lowercase, but that's done on render.
newtype CHeaderName =
  CHeaderName {
    unCHeaderName :: Text
  } deriving (Eq, Show, Generic, Ord)

instance NFData CHeaderName where rnf = genericRnf

newtype CHeaderValue =
  CHeaderValue {
    unCHeaderValue :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData CHeaderValue where rnf = genericRnf

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
    , crHeaders :: !(Map CHeaderName (NonEmpty CHeaderValue))
    , crPayload :: !CPayload
    } deriving (Eq, Show, Generic)

instance NFData CRequest where rnf = genericRnf
