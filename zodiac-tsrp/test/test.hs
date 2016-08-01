import           Disorder.Core.Main

import qualified Test.Zodiac.TSRP.Data.Key
import qualified Test.Zodiac.TSRP.MAC

main :: IO ()
main =
  disorderMain [
    Test.Zodiac.TSRP.Data.Key.tests
  , Test.Zodiac.TSRP.MAC.tests
  ]
