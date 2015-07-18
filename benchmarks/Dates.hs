import Criterion.Main
import Data.Aeson (decode)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime)
import qualified Data.ByteString.Lazy as BL

utcTime :: BL.ByteString -> Maybe [UTCTime]
utcTime = decode

zTime :: BL.ByteString -> Maybe [ZonedTime]
zTime = decode

main :: IO ()
main = do
  let file1 = "json-data/dates.json"
  let file2 = "json-data/dates-fract.json"
  defaultMain [
      bgroup "UTCTime" [
        env (BL.readFile file1) $ \ ~bs -> bench "whole" $ nf utcTime bs
      , env (BL.readFile file2) $ \ ~bs -> bench "fractional" $ nf utcTime bs
      ]
    , bgroup "ZonedTime" [
        env (BL.readFile file1) $ \ ~bs -> bench "whole" $ nf zTime bs
      , env (BL.readFile file2) $ \ ~bs -> bench "fractional" $ nf zTime bs
      ]
    ]
