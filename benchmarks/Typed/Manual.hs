module Typed.Manual
    (
      encodeDirect
    , encodeViaValue
    ) where

import Twitter.Manual
import Data.Aeson hiding (Result)
import Data.ByteString.Lazy as L
import Data.ByteString.Builder as B

encodeDirect :: Result -> L.ByteString
encodeDirect = B.toLazyByteString . toEncoding

encodeViaValue :: Result -> L.ByteString
encodeViaValue = encode
