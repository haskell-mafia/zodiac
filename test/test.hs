import           Disorder.Core.Main

import qualified Test.Zodiac.Data.Request
import qualified Test.Zodiac.Request

main :: IO ()
main =
  disorderMain [
    Test.Zodiac.Data.Request.tests
  , Test.Zodiac.Request.tests
  ]
