{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Zodiac.Core.Data.Key(
    KeyId(..)
  , parseKeyId
  , renderKeyId
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding as T

import           GHC.Generics (Generic)

import           P

import           Tinfoil.Encode (hexEncode)

-- | Identifier for either a symmetric or asymmetric key. Should be
-- globally unique. Sixteen bytes long.
newtype KeyId =
  KeyId {
    unKeyId :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData KeyId where rnf = genericRnf

renderKeyId :: KeyId -> ByteString
renderKeyId = T.encodeUtf8 . hexEncode . unKeyId

-- This should live in tinfoil maybe, as
-- `hexDecode :: Int -> Text -> Maybe' ByteString`?
parseKeyId :: ByteString -> Maybe' KeyId
parseKeyId bs = case B16.decode bs of
  (x, "") -> if BS.length x == 16
               then Just' $ KeyId x
               else Nothing'
  _ -> Nothing'
