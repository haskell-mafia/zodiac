import           Disorder.Core.Main

import qualified Test.Zodiac.Raw.Request

main :: IO ()
main =
  disorderMain [
    Test.Zodiac.Raw.Request.tests
  ]
