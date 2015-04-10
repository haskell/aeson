{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
module Data.Aeson.TypeLits (AtKey(..), getAtKey, mkAtKey, AtIdx(..), getAtIdx, Proxy(..)) where

import           Control.Applicative ((<$>))
import           Control.DeepSeq (NFData(..))
import           Data.Aeson (toJSON, object, (.=), (.:), ToJSON, toJSON, FromJSON, parseJSON)
import           Data.Aeson.Types (Value(..), Parser)
import           Data.Aeson.Types.Instances (typeMismatch)
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T (pack)
import           Data.Typeable (Typeable)
import qualified Data.Vector as V
import           GHC.TypeLits (KnownSymbol, KnownNat, Nat, Symbol, symbolVal, natVal)

-- | Using @AtKey@ you can pick single value from the object
--
-- > >>> decode "{\"name\":\"Joe\"}" :: Maybe (AtKey "name" String)
-- > Just (AtKey "Joe")
--
-- > >>> encode (AtKey 'a' :: AtKey "key" Char)
-- > "{\"key\":\"a\"}"
--
-- For example, given this JSON data:
--
-- > { "name": { "first": "Joe" }, "age": 12 }
--
-- we create a matching data type:
--
-- > data Person = Person
-- >     { name :: Text
-- >     , age  :: Int
-- >     } deriving Show
--
-- To decode data, we need to define a 'FromJSON' instance:
--
-- > {-# LANGUAGE DataKinds #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > instance FromJSON Person where
-- >     parseJSON (Object v) = Person <$> (getAtKay (Proxy :: Proxy "first") <$> v .: "name")
-- >                                   <*> v .: "age"
-- >     -- A non-Object value is of the wrong type, so fail.
-- >     parseJSON _          = mzero
--
-- We can now parse the JSON data like so:
--
-- > >>> decode "{\"name\":{\"first\":\"Joe\"},\"age\":12}" :: Maybe Person
-- > Just (Person {name = "Joe", age = 12})
--
-- To encode data, we need to define a 'ToJSON' instance:
--
-- > instance ToJSON Person where
-- >     toJSON (Person name age) = object ["name" .= mkAtKey (Proxy :: Proxy "first") name, "age" .= age]
--
-- We can now encode a value like so:
--
-- > >>> encode (Person {name = "Joe", age = 12})
-- > "{\"age\":12,\"name\":{\"first\":\"Joe\"}}"
newtype AtKey (s :: Symbol) a = AtKey a
  deriving (Eq, Show, Typeable)

getAtKey :: Proxy s -> AtKey s a -> a
getAtKey _ (AtKey x) = x

mkAtKey :: Proxy s -> a -> AtKey s a
mkAtKey _ = AtKey

instance NFData a => NFData (AtKey s a) where
  rnf (AtKey a) = rnf a

parseAtKey :: (KnownSymbol s, FromJSON a) => Proxy s -> Value -> Parser (AtKey s a)
parseAtKey proxy (Object v) = AtKey <$> v .: T.pack (symbolVal proxy)
parseAtKey proxy v = typeMismatch ("Object with key " ++ symbolVal proxy) v

toJSONAtKey :: (KnownSymbol s, ToJSON a) => Proxy s -> AtKey s a -> Value
toJSONAtKey proxy (AtKey x) = object [ T.pack (symbolVal proxy) .= x ]

instance (KnownSymbol s, FromJSON a) => FromJSON (AtKey s a) where
  parseJSON = parseAtKey Proxy

instance (KnownSymbol s, ToJSON a) => ToJSON (AtKey s a) where
  toJSON = toJSONAtKey Proxy

-- | Using @AtIdx@ you can pick single value from the array
--
-- > >>> decode "[11, 22, 33, 44, 55]" :: Maybe (AtIdx 4 Int)
-- > Just (AtIdx 55)
newtype AtIdx (n :: Nat) a = AtIdx a
  deriving (Eq, Show, Typeable)

getAtIdx :: Proxy n -> AtIdx n a -> a
getAtIdx _ (AtIdx x) = x

instance NFData a => NFData (AtIdx s a) where
  rnf (AtIdx a) = rnf a

parseAtIdx :: (KnownNat n, FromJSON a) => Proxy n -> Value -> Parser (AtIdx n a)
parseAtIdx proxy (Array v) = case v V.!? idx of
	                           Just v' -> AtIdx <$> parseJSON v'
	                           Nothing -> fail $ "there aren't index " ++ show idx  ++ " in array of length " ++ show (V.length v)
  where idx = fromInteger (natVal proxy)
parseAtIdx proxy v = typeMismatch ("array with idx" ++ show idx) v
  where idx = natVal proxy

instance (KnownNat n, FromJSON a) => FromJSON (AtIdx n a) where
  parseJSON = parseAtIdx Proxy
