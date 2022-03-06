{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Aeson.Key (
    Key,
    fromString,
    toString,
    toText,
    fromText,
    coercionToText,
) where

import Prelude (Eq, Ord, (.), Show (..), String, Maybe (..))

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Data.Data (Data)
import Data.Hashable (Hashable(..))
import Data.Monoid (Monoid(mempty, mappend))
import Data.Semigroup (Semigroup((<>)))
import Data.Text (Text)
import Data.Type.Coercion (Coercion (..))
import Data.Typeable (Typeable)
import Text.Read (Read (..))

import qualified Data.String
import qualified Data.Text as T
import qualified Language.Haskell.TH.Syntax as TH

newtype Key = Key { unKey :: Text }
  deriving (Eq, Ord, Typeable, Data)

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

instance Read Key where
    readPrec = fromString <$> readPrec

instance Show Key where
    showsPrec d (Key k) = showsPrec d k 

instance Data.String.IsString Key where
    fromString = fromString

instance Hashable Key where
    hashWithSalt salt (Key k) = hashWithSalt salt k

instance NFData Key where
    rnf (Key k) = rnf k

instance Semigroup Key where
    Key x <> Key y = Key (x <> y)

instance Monoid Key where
    mempty = Key mempty
    mappend = (<>)

instance TH.Lift Key where
#if MIN_VERSION_text(1,2,4)
    lift (Key k) = [| Key k |]
#else
    lift k = [| fromString k' |] where k' = toString k
#endif

#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif
