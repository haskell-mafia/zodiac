{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Zodiac.Raw.Error(
    RequestError(..)
  , renderRequestError
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)
import qualified Data.Text as T

import           GHC.Generics (Generic)

import           P

data RequestError =
    InvalidHTTPMethod !ByteString
  | HeaderNameInvalidUTF8 !ByteString
  | URIInvalidUTF8 !ByteString
  deriving (Eq, Show, Generic)

instance NFData RequestError where rnf = genericRnf

renderRequestError :: RequestError -> Text
renderRequestError (InvalidHTTPMethod m) = T.unwords [
    "invalid HTTP method:"
  , T.pack (show m)
  ]
renderRequestError (HeaderNameInvalidUTF8 bs) = T.unwords [
    "header name not valid UTF-8:"
  , T.pack (show bs)
  ]
renderRequestError (URIInvalidUTF8 bs) = T.unwords [
    "URI not valid UTF-8:"
  , T.pack (show bs)
  ]
