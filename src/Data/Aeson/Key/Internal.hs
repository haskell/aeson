{-# LANGUAGE DeriveDataTypeable #-}

module Data.Aeson.Key.Internal where

import Data.Data (Data)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Prelude (Eq, Ord)

newtype Key = Key { unKey :: Text }
  deriving (Eq, Ord, Typeable, Data)
