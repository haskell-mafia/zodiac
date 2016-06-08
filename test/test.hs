import           Disorder.Core.Main

import qualified Test.Zodiac.Data.Protocol
import qualified Test.Zodiac.Data.Request
import qualified Test.Zodiac.MAC
import qualified Test.Zodiac.Request
import qualified Test.Zodiac.Request.HttpClient

main :: IO ()
main =
  disorderMain [
    Test.Zodiac.Data.Protocol.tests
  , Test.Zodiac.Data.Request.tests
  , Test.Zodiac.MAC.tests
  , Test.Zodiac.Request.tests
  , Test.Zodiac.Request.HttpClient.tests
  ]
