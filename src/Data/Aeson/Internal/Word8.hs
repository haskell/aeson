{-# LANGUAGE PatternSynonyms #-}
module Data.Aeson.Internal.Word8 where

import Data.Word (Word8)

-------------------------------------------------------------------------------
-- Word8 ASCII codes as patterns
-------------------------------------------------------------------------------

-- GHC-8.0 doesn't support giving multiple pattern synonyms type signature at once

-- spaces
pattern W8_SPACE :: Word8
pattern W8_NL    :: Word8
pattern W8_CR    :: Word8
pattern W8_TAB   :: Word8

pattern W8_SPACE = 0x20
pattern W8_NL    = 0x0a
pattern W8_CR    = 0x0d
pattern W8_TAB   = 0x09

-- punctuation
pattern W8_BACKSLASH    :: Word8
pattern W8_DOUBLE_QUOTE :: Word8
pattern W8_DOT          :: Word8
pattern W8_COMMA        :: Word8
pattern W8_COLON        :: Word8

pattern W8_BACKSLASH    = 92
pattern W8_COMMA        = 44
pattern W8_DOT          = 46
pattern W8_DOUBLE_QUOTE = 34
pattern W8_COLON        = 58

-- parentheses
pattern W8_CLOSE_CURLY  :: Word8
pattern W8_CLOSE_SQUARE :: Word8
pattern W8_OPEN_SQUARE  :: Word8
pattern W8_OPEN_CURLY   :: Word8

pattern W8_OPEN_CURLY   = 123
pattern W8_OPEN_SQUARE  = 91
pattern W8_CLOSE_CURLY  = 125
pattern W8_CLOSE_SQUARE = 93

-- operators
pattern W8_MINUS :: Word8
pattern W8_PLUS  :: Word8

pattern W8_PLUS  = 43
pattern W8_MINUS = 45

-- digits
pattern W8_0 :: Word8
pattern W8_9 :: Word8

pattern W8_0 = 48
pattern W8_9 = 57

-- lower case
pattern W8_e :: Word8
pattern W8_f :: Word8
pattern W8_n :: Word8
pattern W8_t :: Word8

pattern W8_e = 101
pattern W8_f = 102
pattern W8_n = 110
pattern W8_t = 116

-- upper case
pattern W8_E :: Word8
pattern W8_E = 69
