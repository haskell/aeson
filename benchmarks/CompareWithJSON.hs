{-# OPTIONS_GHC -fno-warn-orphans#-}

import Criterion.Main
import Control.DeepSeq
import Data.Int
import qualified Data.ByteString.Lazy as BL
import qualified Text.JSON as J
import qualified Data.Aeson as A

instance (NFData v) => NFData (J.JSObject v) where
  rnf o = rnf (J.fromJSObject o)

instance NFData J.JSValue where
  rnf J.JSNull = ()
  rnf (J.JSBool b) = rnf b
  rnf (J.JSRational a b) = rnf a `seq` rnf b `seq` ()
  rnf (J.JSString s) = rnf (J.fromJSString s)
  rnf (J.JSArray lst) = rnf lst
  rnf (J.JSObject o) = rnf o

encodeJ :: J.JSObject J.JSValue -> Int
encodeJ = length . J.encode

encodeA :: A.Value -> Int64
encodeA = BL.length . A.encode

decodeJ :: String -> J.JSObject J.JSValue
decodeJ s =
  case J.decodeStrict s of
    J.Ok v -> v
    J.Error _ -> error "fail to parse via JSON"

decodeA :: BL.ByteString -> A.Value
decodeA s = case A.decode' s of
              Nothing -> error "fail to parse via Aeson"
              Just v -> v

jsonData :: FilePath
jsonData = "benchmarks/json-data/jp100.json"

main :: IO ()
main = do
  js <- readFile jsonData
  as <- BL.readFile jsonData
  let jdata = decodeJ js
      adata = decodeA as
  (jdata, adata) `deepseq` defaultMain [
        bgroup "decode" [ bench "json"  $ nf decodeJ js
                        , bench "aeson" $ nf decodeA as
                        ],
        bgroup "encode" [ bench "json"  $ nf encodeJ jdata
                        , bench "aeson" $ nf encodeA adata
                        ]
       ]
