import           Disorder.Core.Main

import qualified Test.Zodiac.Raw.Request
import qualified Test.Zodiac.Raw.TSRP

main :: IO ()
main =
  disorderMain [
    Test.Zodiac.Raw.Request.tests
  , Test.Zodiac.Raw.TSRP.tests
  ]
