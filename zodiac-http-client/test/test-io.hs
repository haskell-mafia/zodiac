import           Disorder.Core.Main

import qualified Test.IO.Zodiac.HttpClient.TSRP

main :: IO ()
main =
  disorderMain [
    Test.IO.Zodiac.HttpClient.TSRP.tests
  ]
