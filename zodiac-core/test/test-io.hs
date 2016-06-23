import           Disorder.Core.Main

import qualified Test.IO.Zodiac.Core.Data.Symmetric
import qualified Test.IO.Zodiac.Core.Key
import qualified Test.IO.Zodiac.Core.Symmetric
import qualified Test.IO.Zodiac.Core.TSRP.HttpClient

main :: IO ()
main =
  disorderMain [
    Test.IO.Zodiac.Core.Data.Symmetric.tests
  , Test.IO.Zodiac.Core.Key.tests
  , Test.IO.Zodiac.Core.Symmetric.tests
  , Test.IO.Zodiac.Core.TSRP.HttpClient.tests
  ]
