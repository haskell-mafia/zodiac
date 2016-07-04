import           Disorder.Core.Main

import qualified Test.IO.Zodiac.Raw.TSRP

main :: IO ()
main =
  disorderMain [
    Test.IO.Zodiac.Raw.TSRP.tests
  ]
