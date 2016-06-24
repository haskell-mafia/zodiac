import           Disorder.Core.Main

import qualified Test.Zodiac.HttpClient.Request
import qualified Test.Zodiac.HttpClient.TSRP

main :: IO ()
main =
  disorderMain [
    Test.Zodiac.HttpClient.Request.tests
  , Test.Zodiac.HttpClient.TSRP.tests
  ]
