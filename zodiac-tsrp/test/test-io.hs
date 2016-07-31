import           Disorder.Core.Main

import qualified Test.IO.Zodiac.TSRP.Data.Symmetric
import qualified Test.IO.Zodiac.TSRP.Key
import qualified Test.IO.Zodiac.TSRP.Symmetric

main :: IO ()
main =
  disorderMain [
    Test.IO.Zodiac.TSRP.Data.Symmetric.tests
  , Test.IO.Zodiac.TSRP.Key.tests
  , Test.IO.Zodiac.TSRP.Symmetric.tests
  ]
