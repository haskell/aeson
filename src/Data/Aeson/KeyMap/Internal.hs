{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Aeson.KeyMap.Internal where

import Data.Aeson.Key (Key)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Prelude (Eq, Ord, Functor)


#ifdef USE_ORDEREDMAP

import Data.Map (Map)

-- | A map from JSON key type 'Key' to 'v'.
newtype KeyMap v = KeyMap { unKeyMap :: Map Key v }
  deriving (Eq, Ord, Typeable, Data, Functor)

#else

import Data.HashMap.Strict (HashMap)

-- | A map from JSON key type 'Key' to 'v'.
newtype KeyMap v = KeyMap { unKeyMap :: HashMap Key v }
  deriving (Eq, Ord, Typeable, Data, Functor)

#endif
