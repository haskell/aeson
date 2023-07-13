{-# LANGUAGE PatternSynonyms #-}
-- | This is s/Word8/Word16/g copy of .Word8 module.
-- This module is used for low-bit working with text <2 (UTF-16)
module Data.Aeson.Internal.Word16 where

import Data.Word (Word16)

-------------------------------------------------------------------------------
-- Word16 ASCII codes as patterns
-------------------------------------------------------------------------------

-- GHC-8.0 doesn't support giving multiple pattern synonyms type signature at once

-- spaces
pattern W8_SPACE :: Word16
pattern W8_NL    :: Word16
pattern W8_CR    :: Word16
pattern W8_TAB   :: Word16

pattern W8_SPACE = 0x20
pattern W8_NL    = 0x0a
pattern W8_CR    = 0x0d
pattern W8_TAB   = 0x09

-- punctuation
pattern W8_BACKSLASH    :: Word16
pattern W8_DOUBLE_QUOTE :: Word16
pattern W8_DOT          :: Word16
pattern W8_COMMA        :: Word16
pattern W8_COLON        :: Word16

pattern W8_BACKSLASH    = 92
pattern W8_COMMA        = 44
pattern W8_DOT          = 46
pattern W8_DOUBLE_QUOTE = 34
pattern W8_COLON        = 58

-- parentheses
pattern W8_CLOSE_CURLY  :: Word16
pattern W8_CLOSE_SQUARE :: Word16
pattern W8_OPEN_SQUARE  :: Word16
pattern W8_OPEN_CURLY   :: Word16

pattern W8_OPEN_CURLY   = 123
pattern W8_OPEN_SQUARE  = 91
pattern W8_CLOSE_CURLY  = 125
pattern W8_CLOSE_SQUARE = 93

-- operators
pattern W8_MINUS :: Word16
pattern W8_PLUS  :: Word16

pattern W8_PLUS  = 43
pattern W8_MINUS = 45

-- digits
pattern W8_0 :: Word16
pattern W8_9 :: Word16

pattern W8_0 = 48
pattern W8_9 = 57

-- lower case
pattern W8_e :: Word16
pattern W8_f :: Word16
pattern W8_n :: Word16
pattern W8_t :: Word16

pattern W8_e = 101
pattern W8_f = 102
pattern W8_n = 110
pattern W8_t = 116

-- upper case
pattern W8_E :: Word16
pattern W8_E = 69
