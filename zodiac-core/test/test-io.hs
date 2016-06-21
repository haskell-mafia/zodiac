import           Disorder.Core.Main

import qualified Test.IO.Zodiac.Data.Symmetric
import qualified Test.IO.Zodiac.Key
import qualified Test.IO.Zodiac.Symmetric
import qualified Test.IO.Zodiac.TSRP.HttpClient

main :: IO ()
main =
  disorderMain [
    Test.IO.Zodiac.Data.Symmetric.tests
  , Test.IO.Zodiac.Key.tests
  , Test.IO.Zodiac.Symmetric.tests
  , Test.IO.Zodiac.TSRP.HttpClient.tests
  ]
