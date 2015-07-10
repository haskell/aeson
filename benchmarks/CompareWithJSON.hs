{-# OPTIONS_GHC -fno-warn-orphans #-}

import Blaze.ByteString.Builder (toLazyByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Control.DeepSeq (NFData(rnf))
import Criterion.Main
import qualified Data.Aeson.Encode as A
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Text.JSON as J
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Builder  as TLB
import qualified Data.Text.Lazy.Encoding as TLE

instance (NFData v) => NFData (J.JSObject v) where
  rnf o = rnf (J.fromJSObject o)

instance NFData J.JSValue where
  rnf J.JSNull = ()
  rnf (J.JSBool b) = rnf b
  rnf (J.JSRational a b) = rnf a `seq` rnf b `seq` ()
  rnf (J.JSString s) = rnf (J.fromJSString s)
  rnf (J.JSArray lst) = rnf lst
  rnf (J.JSObject o) = rnf o

decodeJ :: String -> J.JSValue
decodeJ s =
  case J.decodeStrict s of
    J.Ok v -> v
    J.Error _ -> error "fail to parse via JSON"

decodeA :: BL.ByteString -> A.Value
decodeA s = case A.decode s of
              Just v -> v
              Nothing -> error "fail to parse via Aeson"

decodeA' :: BL.ByteString -> A.Value
decodeA' s = case A.decode' s of
               Just v -> v
               Nothing -> error "fail to parse via Aeson"

encodeJ :: J.JSValue -> BL.ByteString
encodeJ = toLazyByteString . fromString . J.encode

encodeToText :: A.Value -> TL.Text
encodeToText = TLB.toLazyText . A.encodeToTextBuilder . A.toJSON

encodeViaText :: A.Value -> BL.ByteString
encodeViaText = TLE.encodeUtf8 . encodeToText

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
          bench "aeson/lazy" $ nf decodeA enA
        , bench "aeson/strict" $ nf decodeA' enA
        , bench "json"  $ nf decodeJ enJ
        ]
      , bgroup "jp" [
          bench "aeson" $ nf decodeA jpA
        , bench "json"  $ nf decodeJ jpJ
        ]
      ]
    , bgroup "encode" [
        bgroup "en" [
          bench "aeson-to-bytestring" $ nf A.encode (decodeA enA)
        , bench "aeson-via-text-to-bytestring" $ nf encodeViaText (decodeA enA)
        , bench "aeson-to-text" $ nf encodeToText (decodeA enA)
        , bench "json"  $ nf encodeJ (decodeJ enJ)
        ]
      , bgroup "jp" [
          bench "aeson-to-bytestring" $ nf A.encode (decodeA jpA)
        , bench "aeson-via-text-to-bytestring" $ nf encodeViaText (decodeA jpA)
        , bench "aeson-to-text" $ nf encodeToText (decodeA jpA)
        , bench "json"  $ nf encodeJ (decodeJ jpJ)
        ]
      ]
    ]
