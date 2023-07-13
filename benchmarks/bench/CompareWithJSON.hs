{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CompareWithJSON (benchmark) where

import Prelude.Compat
import Bench

import Blaze.ByteString.Builder (toLazyByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Control.DeepSeq (NFData(rnf))
import Data.Maybe (fromMaybe)
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.Aeson.Parser.Internal as I
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Builder  as TLB
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Text.JSON as J

import Utils

instance (NFData v) => NFData (J.JSObject v) where
  rnf o = rnf (J.fromJSObject o)

instance NFData J.JSValue where
  rnf J.JSNull = ()
  rnf (J.JSBool b) = rnf b
  rnf (J.JSRational a b) = rnf a `seq` rnf b
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
decodeS s = fromMaybe (error "fail to parse via Aeson") $ A.decodeStrict s

decodeS' :: BS.ByteString -> A.Value
decodeS' s = fromMaybe (error "fail to parse via Aeson") $ A.decodeStrict' s

decodeT :: T.Text -> A.Value
decodeT t = fromMaybe (error "fail to parse via Aeson") $ A.decodeStrictText t

decodeTviaBS :: T.Text -> A.Value
decodeTviaBS t = fromMaybe (error "fail to parse via Aeson") $ A.decodeStrict $ TE.encodeUtf8 t

decodeAtto :: BL.ByteString -> A.Value
decodeAtto s = fromMaybe (error "fail to parse via Parser.decodeWith") $
    I.decodeWith I.jsonEOF A.fromJSON s

decodeAtto' :: BL.ByteString -> A.Value
decodeAtto' s = fromMaybe (error "fail to parse via Parser.decodeWith") $
    I.decodeWith I.jsonEOF' A.fromJSON s

decodeAttoS :: BS.ByteString -> A.Value
decodeAttoS s = fromMaybe (error "fail to parse via Parser.decodeWith") $
    I.decodeStrictWith I.jsonEOF A.fromJSON s

decodeAttoS' :: BS.ByteString -> A.Value
decodeAttoS' s = fromMaybe (error "fail to parse via Parser.decodeWith") $
    I.decodeStrictWith I.jsonEOF' A.fromJSON s

encodeJ :: J.JSValue -> BL.ByteString
encodeJ = toLazyByteString . fromString . J.encode

encodeToText :: A.Value -> TL.Text
encodeToText = TLB.toLazyText . A.encodeToTextBuilder . A.toJSON

encodeViaText :: A.Value -> BL.ByteString
encodeViaText = TLE.encodeUtf8 . encodeToText

benchmark :: Benchmark
benchmark =
  env (readL enFile) $ \enA ->
  env (readS enFile) $ \enS ->
  env (readStr enFile) $ \enJ ->
  env (readT enFile) $ \enT ->
  env (readL jpFile) $ \jpA ->
  env (readS jpFile) $ \jpS ->
  env (readStr jpFile) $ \jpJ ->
  env (readT jpFile) $ \jpT ->
  bgroup "compare-json" [
      bgroup "decode" [
        bgroup "whnf" [
          -- Note: we use whnf to only force the outer constructor,
          -- which may force different amount of value substructure.
          bench "aeson/normal"      $ whnf decode enA
        , bench "aeson/normal'"     $ whnf decode' enA
        , bench "aeson/strict"      $ whnf decodeS enS
        , bench "aeson/strict'"     $ whnf decodeS' enS
        , bench "aeson/text"        $ whnf decodeT enT
        , bench "aeson/text-via-bs" $ whnf decodeTviaBS enT

          -- attoparsec-aeson package
        , bench "aeson/atto"        $ whnf decodeAtto enA
        , bench "aeson/atto'"       $ whnf decodeAtto' enA
        , bench "aeson/attoS"       $ whnf decodeAttoS enS
        , bench "aeson/attoS'"      $ whnf decodeAttoS' enS

          -- json package
        , bench "json"              $ whnf decodeJ enJ
        ]

      , bgroup "nf" [
          bench "aeson/normal"      $ nf decode enA
        , bench "aeson/normal"      $ nf decode' enA
        , bench "aeson/strict"      $ nf decodeS enS
        , bench "aeson/strict'"     $ nf decodeS' enS

          -- attoparsec-aeson package
        , bench "aeson/atto"        $ nf decodeAtto enA
        , bench "aeson/atto'"       $ nf decodeAtto' enA
        , bench "aeson/attoS"       $ nf decodeAttoS enS
        , bench "aeson/attoS'"      $ nf decodeAttoS' enS

          -- json package
        , bench "json"              $ nf decodeJ enJ
        ]
      , bgroup "jp" [
          bench "aeson/normal"      $ whnf decode jpA
        , bench "aeson/strict"      $ whnf decodeS jpS
        , bench "aeson/text"        $ whnf decodeT jpT
        , bench "aeson/text-via-bs" $ whnf decodeTviaBS jpT
        , bench "json"              $ whnf decodeJ jpJ
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
  where
    enFile = "twitter100.json"
    jpFile = "jp100.json"
