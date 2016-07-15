import           Disorder.Core.Main

import qualified Test.Zodiac.Export.Time

main :: IO ()
main =
  disorderMain [
    Test.Zodiac.Export.Time.tests
  ]
