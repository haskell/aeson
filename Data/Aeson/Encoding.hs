module Data.Aeson.Encoding
    (
    -- * Encoding
      Encoding
    , fromEncoding
    , unsafeToEncoding
    , Series
    , pairs
    -- * Predicates
    , nullEncoding
    -- * Encoding constructors
    , emptyArray_
    , emptyObject_
    , text
    , list
    , dict
    ) where

import Data.Aeson.Encoding.Internal
