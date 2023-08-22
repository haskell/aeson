module Data.Hermes.Aeson where

import Control.Applicative ((<|>))

import qualified Data.Hermes as H
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

valueDecoder :: H.Value -> H.Decoder A.Value
valueDecoder v =
    A.String               <$> H.text v <|>
    A.Number               <$> H.scientific v <|>
    A.Bool                 <$> H.bool v <|>
    A.Object . KM.fromList <$> H.objectAsKeyValues (pure . K.fromText) valueDecoder v <|>
    A.Array . V.fromList   <$> H.list valueDecoder v <|>
    A.Null                 <$  H.nullable (\_ -> fail "notnull") v
