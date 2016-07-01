import           Disorder.Core.Main

import qualified Test.Zodiac.Core.Data.Protocol
import qualified Test.Zodiac.Core.Data.Request
import qualified Test.Zodiac.Core.Data.Time
import qualified Test.Zodiac.Core.Header
import qualified Test.Zodiac.Core.MAC
import qualified Test.Zodiac.Core.Request
import qualified Test.Zodiac.Core.Time

main :: IO ()
main =
  disorderMain [
    Test.Zodiac.Core.Data.Protocol.tests
  , Test.Zodiac.Core.Data.Request.tests
  , Test.Zodiac.Core.Data.Time.tests
  , Test.Zodiac.Core.Header.tests
  , Test.Zodiac.Core.MAC.tests
  , Test.Zodiac.Core.Request.tests
  , Test.Zodiac.Core.Time.tests
  ]
