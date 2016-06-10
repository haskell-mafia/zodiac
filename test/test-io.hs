import           Disorder.Core.Main

import qualified Test.IO.Zodiac.Symmetric

main :: IO ()
main =
  disorderMain [
    Test.IO.Zodiac.Symmetric.tests
  ]
