
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Time.Clock (UTCTime)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Criterion.Main

utcTime :: BL.ByteString -> [UTCTime]
utcTime str = let Just t = decode str in t

main :: IO ()
main = do
  let file1 = "json-data/dates.json"
  let file2 = "json-data/dates-fract.json"
  enA1 <- BL.readFile file1
  enA2 <- BL.readFile file2
  defaultMain
    [ bench "utc-time" $ nf utcTime enA1
    , bench "utc-time fract" $ nf utcTime enA2
    ]
