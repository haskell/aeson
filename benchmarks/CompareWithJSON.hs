{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.DeepSeq (NFData(rnf))
import Criterion.Main
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Text.JSON as J

instance (NFData v) => NFData (J.JSObject v) where
  rnf o = rnf (J.fromJSObject o)

instance NFData J.JSValue where
  rnf J.JSNull = ()
  rnf (J.JSBool b) = rnf b
  rnf (J.JSRational a b) = rnf a `seq` rnf b `seq` ()
  rnf (J.JSString s) = rnf (J.fromJSString s)
  rnf (J.JSArray lst) = rnf lst
  rnf (J.JSObject o) = rnf o

instance NFData BL.ByteString where
  rnf (BL.Chunk _ bs) = rnf bs
  rnf BL.Empty        = ()

decodeJ :: String -> J.JSValue
decodeJ s =
  case J.decodeStrict s of
    J.Ok v -> v
    J.Error _ -> error "fail to parse via JSON"

decodeA :: BL.ByteString -> A.Value
decodeA s = case A.decode s of
              Just v -> v
              Nothing -> error "fail to parse via Aeson"

main :: IO ()
main = do
  let enFile = "json-data/twitter100.json"
      jpFile = "json-data/jp100.json"
  enA <- BL.readFile enFile
  enJ <- readFile enFile
  jpA <- BL.readFile jpFile
  jpJ <- readFile jpFile
  defaultMain [
      bgroup "decode" [
        bgroup "en" [
          bench "aeson" $ nf decodeA enA
        , bench "json"  $ nf decodeJ enJ
        ]
      , bgroup "jp" [
          bench "aeson" $ nf decodeA jpA
        , bench "json"  $ nf decodeJ jpJ
        ]
      ]
    , bgroup "encode" [
        bgroup "en" [
          bench "aeson" $ nf A.encode (decodeA enA)
        , bench "json"  $ nf J.encode (decodeJ enJ)
        ]
      , bgroup "jp" [
          bench "aeson" $ nf A.encode (decodeA jpA)
        , bench "json"  $ nf J.encode (decodeJ jpJ)
        ]
      ]
    ]
