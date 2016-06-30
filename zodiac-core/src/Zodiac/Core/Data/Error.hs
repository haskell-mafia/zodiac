{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Zodiac.Core.Data.Error(
    ProtocolError(..)
  , renderProtocolError
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           GHC.Generics (Generic)

import           P

-- | Generic protocol errors. If we encounter one of these we always
-- terminate immediately; clients should log these errors but never
-- expose them to users.
data ProtocolError =
    NoAuthHeader
  | MalformedAuthHeader
  | MultipleAuthHeaders
  deriving (Eq, Show, Generic)

instance NFData ProtocolError where rnf = genericRnf

renderProtocolError :: ProtocolError -> Text
renderProtocolError NoAuthHeader = "no Authorization header present"
renderProtocolError MalformedAuthHeader = "invalid Authorization header"
renderProtocolError MultipleAuthHeaders = "multiple Authorization header values present"
