import           Disorder.Core.Main

import qualified Test.Zodiac.Core.Data.Protocol
import qualified Test.Zodiac.Core.Data.Request
import qualified Test.Zodiac.Core.Data.Time
import qualified Test.Zodiac.Core.MAC
import qualified Test.Zodiac.Core.Request
import qualified Test.Zodiac.Core.Request.HttpClient
import qualified Test.Zodiac.Core.Time
import qualified Test.Zodiac.Core.TSRP.HttpClient

main :: IO ()
main =
  disorderMain [
    Test.Zodiac.Core.Data.Protocol.tests
  , Test.Zodiac.Core.Data.Request.tests
  , Test.Zodiac.Core.Data.Time.tests
  , Test.Zodiac.Core.MAC.tests
  , Test.Zodiac.Core.Request.tests
  , Test.Zodiac.Core.Request.HttpClient.tests
  , Test.Zodiac.Core.Time.tests
  , Test.Zodiac.Core.TSRP.HttpClient.tests
  ]
