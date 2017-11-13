{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Prelude ()
import Prelude.Compat

import Blaze.ByteString.Builder (toLazyByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Control.DeepSeq (NFData(rnf))
import Criterion.Main
import Data.Maybe (fromMaybe)
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.Aeson.Parser.Internal as I
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Builder  as TLB
import qualified Data.Text.Lazy.Encoding as TLE
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

decodeJ :: String -> J.JSValue
decodeJ s =
  case J.decodeStrict s of
    J.Ok v -> v
    J.Error _ -> error "fail to parse via JSON"

decode :: BL.ByteString -> A.Value
decode s = fromMaybe (error "fail to parse via Aeson") $ A.decode s

decode' :: BL.ByteString -> A.Value
decode' s = fromMaybe (error "fail to parse via Aeson") $ A.decode' s

decodeS :: BS.ByteString -> A.Value
decodeS s = fromMaybe (error "fail to parse via Aeson") $ A.decodeStrict' s

decodeIP :: BL.ByteString -> A.Value
decodeIP s = fromMaybe (error "fail to parse via Parser.decodeWith") $
    I.decodeWith I.jsonEOF A.fromJSON s

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
  enS <- BS.readFile enFile
  enJ <- readFile enFile
  jpA <- BL.readFile jpFile
  jpS <- BS.readFile jpFile
  jpJ <- readFile jpFile
  defaultMain [
      bgroup "decode" [
        bgroup "en" [
          bench "aeson/lazy"     $ nf decode enA
        , bench "aeson/strict"   $ nf decode' enA
        , bench "aeson/stricter" $ nf decodeS enS
        , bench "aeson/parser"   $ nf decodeIP enA
        , bench "json"           $ nf decodeJ enJ
        ]
      , bgroup "jp" [
          bench "aeson"          $ nf decode jpA
        , bench "aeson/stricter" $ nf decodeS jpS
        , bench "json"           $ nf decodeJ jpJ
        ]
      ]
    , bgroup "encode" [
        bgroup "en" [
          bench "aeson-to-bytestring" $ nf A.encode (decode enA)
        , bench "aeson-via-text-to-bytestring" $ nf encodeViaText (decode enA)
        , bench "aeson-to-text" $ nf encodeToText (decode enA)
        , bench "json"  $ nf encodeJ (decodeJ enJ)
        ]
      , bgroup "jp" [
          bench "aeson-to-bytestring" $ nf A.encode (decode jpA)
        , bench "aeson-via-text-to-bytestring" $ nf encodeViaText (decode jpA)
        , bench "aeson-to-text" $ nf encodeToText (decode jpA)
        , bench "json"  $ nf encodeJ (decodeJ jpJ)
        ]
      ]
    ]
