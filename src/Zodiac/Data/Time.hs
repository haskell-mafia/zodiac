{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Zodiac.Data.Time(
    RequestDate(..)
  , RequestExpired(..)
  , RequestExpiry(..)
  , RequestTimestamp(..)

  , parseRequestExpiry
  , parseRequestTimestamp
  , renderRequestDate
  , renderRequestExpiry
  , renderRequestTimestamp
  , timestampDate
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as ABC
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.Encoding as T
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Format (formatTime, iso8601DateFormat)
import           Data.Time.Format (defaultTimeLocale, parseTimeM)


import           GHC.Generics (Generic)

import           P

-- | Whether the request being processed is beyond its expiry window.
data RequestExpired =
    RequestExpired
  | RequestNotExpired
  deriving (Eq, Show, Generic)

instance NFData RequestExpired where rnf = genericRnf

-- | Time at which the request is made. Precision is to the second.
newtype RequestTimestamp =
  RequestTimestamp {
    unRequestTimestamp :: UTCTime
  } deriving (Eq, Generic)

instance NFData RequestTimestamp where rnf = genericRnf

instance Show RequestTimestamp where
  show = BSC.unpack . renderRequestTimestamp

requestTimestampFormat :: [Char]
requestTimestampFormat =
  iso8601DateFormat $ Just "%H:%M:%S"

renderRequestTimestamp :: RequestTimestamp -> ByteString
renderRequestTimestamp (RequestTimestamp ts) =
  let str = formatTime defaultTimeLocale requestTimestampFormat ts in
  BSC.pack str

parseRequestTimestamp :: ByteString -> Maybe' RequestTimestamp
parseRequestTimestamp =
  fmap RequestTimestamp . strictMaybe .
    parseTimeM False defaultTimeLocale requestTimestampFormat . BSC.unpack

-- | Date on which a request is made (UTC).
newtype RequestDate =
  RequestDate {
    unRequestDate :: Day
  } deriving (Eq, Generic)

instance NFData RequestDate where rnf = genericRnf

instance Show RequestDate where
  show = BSC.unpack . renderRequestDate

timestampDate :: RequestTimestamp -> RequestDate
timestampDate = RequestDate . utctDay . unRequestTimestamp

-- | Render just the date part in ISO-8601 format.
renderRequestDate :: RequestDate -> ByteString
renderRequestDate (RequestDate rd) =
  let ts = UTCTime rd 0
      fmt = iso8601DateFormat Nothing
      str = formatTime defaultTimeLocale fmt ts in
  BSC.pack str

-- | Number of seconds for a request to be considered valid - after
-- this time, an application server will discard it.
newtype RequestExpiry =
  RequestExpiry {
    unRequestExpiry :: Int
  } deriving (Eq, Show, Generic)

instance NFData RequestExpiry where rnf = genericRnf

renderRequestExpiry :: RequestExpiry -> ByteString
renderRequestExpiry = T.encodeUtf8 . renderIntegral . unRequestExpiry

parseRequestExpiry :: ByteString -> Maybe' RequestExpiry
parseRequestExpiry bs =
  case AB.parseOnly (ABC.decimal <* AB.endOfInput) bs of
    Right x -> if x > 0
                 then pure $ RequestExpiry x
                 else Nothing'
    -- Don't want to propagate errors up from 'symmetricAuthHeaderP' at this point.
    Left _ -> Nothing'
