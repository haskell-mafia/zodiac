import           Disorder.Core.Main

import qualified Test.Zodiac.TSRP.MAC

main :: IO ()
main =
  disorderMain [
    Test.Zodiac.TSRP.MAC.tests
  ]
