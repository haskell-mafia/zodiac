import           Disorder.Core.Main

import qualified Test.IO.Zodiac.Key
import qualified Test.IO.Zodiac.Symmetric

main :: IO ()
main =
  disorderMain [
    Test.IO.Zodiac.Key.tests
  , Test.IO.Zodiac.Symmetric.tests
  ]
