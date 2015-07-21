import Control.Applicative ((<$>))
import Criterion.Main
import Data.Aeson (decode, encode)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime)
import qualified Data.ByteString.Lazy as BL

utcTime :: BL.ByteString -> Maybe [UTCTime]
utcTime = decode

zTime :: BL.ByteString -> Maybe [ZonedTime]
zTime = decode

main :: IO ()
main = do
  let file1 = BL.readFile "json-data/dates.json"
  let file2 = BL.readFile "json-data/dates-fract.json"
  defaultMain [
      bgroup "decode" [
        bgroup "UTCTime" [
          env file1 $ \ ~bs -> bench "whole" $ nf utcTime bs
        , env file2 $ \ ~bs -> bench "fractional" $ nf utcTime bs
        ]
      , bgroup "ZonedTime" [
          env file1 $ \ ~bs -> bench "whole" $ nf zTime bs
        , env file2 $ \ ~bs -> bench "fractional" $ nf zTime bs
        ]
      ]
    , bgroup "encode" [
        bgroup "UTCTime" [
          env (utcTime <$> file1) $ \ ~ts -> bench "whole" $ nf encode ts
        , env (utcTime <$> file2) $ \ ~ts -> bench "fractional" $ nf encode ts
        ]
      , bgroup "ZonedTime" [
          env (zTime <$> file1) $ \ ~ts -> bench "whole" $ nf encode ts
        , env (zTime <$> file2) $ \ ~ts -> bench "fractional" $ nf encode ts
        ]
      ]
    ]
