{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Parser
    (
      json
    ) where

import Control.Applicative as A
import Data.Attoparsec.Char8
import Data.Aeson.Types (Value(..))
import Data.Aeson.Parser.Internal (array, object)

json :: Parser Value
json = do
  skipSpace
  c <- anyChar
  case c of
    '{' -> skipSpace *> object
    '[' -> skipSpace *> array
    _   -> fail "root value is not an object or array"
