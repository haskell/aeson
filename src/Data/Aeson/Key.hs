{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Aeson.Key (
    Key,
    fromString,
    toString,
    toText,
    fromText,
    coercionToText,
) where

import Prelude ((.), String, Maybe (..), id)

import Data.Text (Text)
import Data.Type.Coercion (Coercion (..))

import qualified Data.Text as T
import qualified Language.Haskell.TH.Syntax as TH

type Key = Text

unKey :: Key -> Text
unKey = id

pattern Key :: p -> p
pattern Key a <- a
  where Key a = a

fromString :: String -> Key
fromString = Key . T.pack

toString :: Key -> String
toString (Key k) = T.unpack k

fromText :: Text -> Key
fromText = Key

toText :: Key -> Text
toText = unKey

-- | @'coercing r1 r2'@ will evaluate to @r1@ if 'Key' is 'Coercible' to  'Text',
-- and to @r2@ otherwise.
--
-- Using 'coercing' we can make more efficient implementations
-- when 'Key' is backed up by 'Text' without exposing internals.
-- 
coercionToText :: Maybe (Coercion Key Text)
coercionToText = Just Coercion
{-# INLINE coercionToText #-}

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

-- No instances for type synonym
