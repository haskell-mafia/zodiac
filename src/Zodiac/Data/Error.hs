{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Zodiac.Data.Error(
    RequestError(..)
  , renderRequestError
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)
import qualified Data.Text as T

import           GHC.Generics (Generic)

import           P

data RequestError =
    InvalidHTTPMethod ByteString
  | UnsupportedPayloadType
  deriving (Eq, Show, Generic)

renderRequestError :: RequestError -> Text
renderRequestError (InvalidHTTPMethod m) = T.unwords [
    "invalid HTTP method:"
  , T.pack (show m)
  ]
renderRequestError UnsupportedPayloadType =
  "unsupported request payload type"

instance NFData RequestError where rnf = genericRnf
