{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Zodiac.Data.Key(
    KeyId(..)
  , renderKeyId
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

import           Tinfoil.Encode (hexEncode)

-- | Identifier for either a symmetric or asymmetric key. Should be
-- globally unique.
newtype KeyId =
  KeyId {
    unKeyId :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData KeyId where rnf = genericRnf

renderKeyId :: KeyId -> Text
renderKeyId = hexEncode . unKeyId
