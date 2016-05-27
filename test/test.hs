import           Disorder.Core.Main

import qualified Test.Zodiac.Data.Request

main :: IO ()
main =
  disorderMain [
    Test.Zodiac.Data.Request.tests
  ]
