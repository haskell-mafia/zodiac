{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Zodiac.TSRP.Data.Key(
    KeyId(..)
  , parseKeyId
  , renderKeyId
  , tsrpKeyIdLength

  , TSRPKey(..)
  , parseTSRPKey
  , renderTSRPKey
  , tsrpSecretKeyLength
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding as T

import           GHC.Generics (Generic)

import           P

import           Tinfoil.Data.Key (SymmetricKey(..))
import           Tinfoil.Data.Key (parseSymmetricKey, renderSymmetricKey)
import           Tinfoil.Encode (hexEncode)

-- | Identifier for a TSRP key. Should be globally unique. Sixteen bytes long
-- plus six bytes for the tag prefix.
newtype KeyId =
  KeyId {
    unKeyId :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData KeyId where rnf = genericRnf

-- | "TSRPV1" + "KEYID" mod 26 + 65.
tsrpKeyIdTag :: ByteString
tsrpKeyIdTag = "DWPXY1"

renderKeyId :: KeyId -> ByteString
renderKeyId (KeyId bs) =
  tsrpKeyIdTag <> (T.encodeUtf8 $ hexEncode bs)

parseKeyId :: ByteString -> Maybe' KeyId
parseKeyId bs =
  let (h, t) = BS.splitAt (BS.length tsrpKeyIdTag) bs in
  case h == tsrpKeyIdTag of
    True -> case B16.decode t of
      (x, "") -> case BS.length x == tsrpKeyIdLength of
        True -> Just' $ KeyId x
        False -> Nothing'
      _ -> Nothing'
    False ->
      Nothing'

-- | Key ID length in bytes (raw, not encoded). This is the in-memory
-- representation, not the rendered version.
tsrpKeyIdLength :: Int
tsrpKeyIdLength = 16

-- | Key length in bytes (raw, not encoded). This is the in-memory
-- representation, not the rendering.
tsrpSecretKeyLength :: Int
tsrpSecretKeyLength = 32

newtype TSRPKey =
  TSRPKey {
    unTSRPKey :: SymmetricKey
  } deriving (Eq, Generic)

instance NFData TSRPKey where rnf = genericRnf

-- | "TSRPV1" + "SECRET" mod 26 + 65.
tsrpKeyTag :: ByteString
tsrpKeyTag = "LWTGZD"

renderTSRPKey :: TSRPKey -> ByteString
renderTSRPKey (TSRPKey sk) =
  tsrpKeyTag <> (T.encodeUtf8 $ renderSymmetricKey sk)

parseTSRPKey :: ByteString -> Maybe' TSRPKey
parseTSRPKey bs =
  let (h, t) = BS.splitAt (BS.length tsrpKeyTag) bs in
  case h == tsrpKeyTag of
    True ->
      case T.decodeUtf8' t of
        Left _ ->
          Nothing'
        Right t' ->
          fmap TSRPKey $ parseSymmetricKey t'
    False ->
      Nothing'
