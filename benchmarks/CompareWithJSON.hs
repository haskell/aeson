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

jsonData :: FilePath
jsonData = "json-data/jp100.json"

main :: IO ()
main = do
  js <- readFile jsonData
  as <- BL.readFile jsonData
  let jdata = decodeJ js
      adata = decodeA as
  defaultMain [
        bgroup "decode" [ bench "json"  $ nf decodeJ js
                        , bench "aeson" $ nf decodeA as
                        ],
        bgroup "encode" [ bench "json"  $ nf J.encode jdata
                        , bench "aeson" $ nf A.encode adata
                        ]
       ]
