{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}

-- Module:      Data.Aeson.Types
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types
    (
    -- * Core JSON types
      Value(..)
    , Array
    , emptyArray
    , Pair
    , Object
    , emptyObject
    -- * Type conversion
    , FromJSON(..)
    , ToJSON(..)
    -- * Constructors and accessors
    , (.=)
    , (.:)
    , (.:?)
    , object
    ) where

import Control.Arrow ((***))
import Control.Applicative
import Control.DeepSeq (NFData(..))
import Data.Data (Data)
import Data.Map (Map)
import Data.Monoid (Dual(..), First(..), Last(..))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, parseTime)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V

-- | A JSON \"object\" (key\/value map).
type Object = Map Text Value

-- | A JSON \"array\" (sequence).
type Array = Vector Value

-- | A JSON value represented as a Haskell value.
data Value = Object Object
           | Array Array
           | String Text
           | Number Double
           | Bool !Bool
           | Null
             deriving (Eq, Show, Typeable, Data)

instance NFData Value where
    rnf (Object o) = rnf o
    rnf (Array a)  = V.foldl' (\x y -> rnf y `seq` x) () a
    rnf (String s) = rnf s
    rnf (Number n) = rnf n
    rnf (Bool b)   = rnf b
    rnf Null       = ()

-- | The empty array.
emptyArray :: Value
emptyArray = Array V.empty

-- | The empty object.
emptyObject :: Value
emptyObject = Object M.empty

-- | A key\/value pair for an 'Object'.
newtype Pair = Pair { unPair :: (Text, Value) }
    deriving (Eq, Typeable)

instance Show Pair where
    show = show . unPair

-- | Construct a 'Pair' from a key and a value.
(.=) :: ToJSON a => Text -> a -> Pair
name .= value = Pair (name, toJSON value)
{-# INLINE (.=) #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '(.:?)' instead.
(.:) :: (Alternative m, FromJSON a) => Object -> Text -> m a
obj .: key = case M.lookup key obj of
               Nothing -> empty
               Just v  -> fromJSON v
{-# INLINE (.:) #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present, or 'empty' if
-- the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '(.:?)' instead.
(.:?) :: (Alternative m, FromJSON a) => Object -> Text -> m (Maybe a)
obj .:? key = case M.lookup key obj of
               Nothing -> pure Nothing
               Just v  -> fromJSON v
{-# INLINE (.:?) #-}

-- | Create a 'Value' from a list of 'Pair's.  If duplicate
-- keys arise, earlier keys and their associated values win.
object :: [Pair] -> Value
object = Object . M.fromList . map unPair
{-# INLINE object #-}

-- | A type that can be converted to JSON.
--
-- An example type and instance:
--
-- @data Coord { x :: Double, y :: Double }
--
-- instance ToJSON Coord where
--   toJSON (Coord x y) = 'object' [\"x\" '.=' x, \"y\" '.=' y]
-- @
class ToJSON a where
    toJSON   :: a -> Value

-- | A type that can be converted from JSON, with the possibility of
-- failure.
--
-- When writing an instance, use 'mzero' to make a conversion fail,
-- e.g. if an 'Object' is missing a required key, or the value is of
-- the wrong type.
--
-- An example type and instance:
--
-- @data Coord { x :: Double, y :: Double }
-- 
-- instance FromJSON Coord where
--   fromJSON ('Object' v) = Coord '<$>'
--                         v '.:' \"x\" '<*>'
--                         v '.:' \"y\"
--
--   \-- A non-'Object' value is of the wrong type, so use 'mzero' to fail.
--   fromJSON _          = 'mzero'
-- @
class FromJSON a where
    fromJSON :: Alternative m => Value -> m a

instance (ToJSON a) => ToJSON (Maybe a) where
    toJSON (Just a) = toJSON a
    toJSON Nothing  = Null
    {-# INLINE toJSON #-}
    
instance (FromJSON a) => FromJSON (Maybe a) where
    fromJSON Null   = pure Nothing
    fromJSON a      = Just <$> fromJSON a
    {-# INLINE fromJSON #-}

instance (ToJSON a, ToJSON b) => ToJSON (Either a b) where
    toJSON (Left a)  = toJSON a
    toJSON (Right b) = toJSON b
    {-# INLINE toJSON #-}
    
instance (FromJSON a, FromJSON b) => FromJSON (Either a b) where
    fromJSON a = Left <$> fromJSON a <|> Right <$> fromJSON a
    {-# INLINE fromJSON #-}

instance ToJSON Bool where
    toJSON = Bool
    {-# INLINE toJSON #-}

instance FromJSON Bool where
    fromJSON (Bool b) = pure b
    fromJSON _        = empty
    {-# INLINE fromJSON #-}

instance ToJSON Double where
    toJSON = Number
    {-# INLINE toJSON #-}

instance FromJSON Double where
    fromJSON (Number n) = pure n
    fromJSON _          = empty
    {-# INLINE fromJSON #-}

instance ToJSON Int where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Int where
    fromJSON (Number n) = pure (floor n)
    fromJSON _          = empty
    {-# INLINE fromJSON #-}

instance ToJSON Integer where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Integer where
    fromJSON (Number n) = pure (floor n)
    fromJSON _          = empty
    {-# INLINE fromJSON #-}

instance ToJSON Text where
    toJSON = String
    {-# INLINE toJSON #-}

instance FromJSON Text where
    fromJSON (String t) = pure t
    fromJSON _          = empty
    {-# INLINE fromJSON #-}

instance ToJSON LT.Text where
    toJSON = String . LT.toStrict
    {-# INLINE toJSON #-}

instance FromJSON LT.Text where
    fromJSON (String t) = pure (LT.fromStrict t)
    fromJSON _          = empty
    {-# INLINE fromJSON #-}

instance ToJSON B.ByteString where
    toJSON = String . decodeUtf8
    {-# INLINE toJSON #-}

instance FromJSON B.ByteString where
    fromJSON (String t) = pure . encodeUtf8 $ t
    fromJSON _          = empty
    {-# INLINE fromJSON #-}

instance ToJSON LB.ByteString where
    toJSON = toJSON . B.concat . LB.toChunks
    {-# INLINE toJSON #-}

instance FromJSON LB.ByteString where
    fromJSON (String t) = pure . LB.fromChunks . (:[]) . encodeUtf8 $ t
    fromJSON _          = empty
    {-# INLINE fromJSON #-}

instance (ToJSON a) => ToJSON [a] where
    toJSON = Array . V.fromList . map toJSON
    {-# INLINE toJSON #-}
    
instance (FromJSON a) => FromJSON [a] where
    fromJSON (Array a) = mapA fromJSON (V.toList a)
    fromJSON _         = empty
    {-# INLINE fromJSON #-}

instance (ToJSON a) => ToJSON (Vector a) where
    toJSON = Array . V.map toJSON
    {-# INLINE toJSON #-}
    
instance (FromJSON a) => FromJSON (Vector a) where
    fromJSON (Array a) = V.fromList <$> mapA fromJSON (V.toList a)
    fromJSON _         = empty
    {-# INLINE fromJSON #-}

instance (ToJSON a) => ToJSON (Set.Set a) where
    toJSON = toJSON . Set.toList
    {-# INLINE toJSON #-}
    
instance (Ord a, FromJSON a) => FromJSON (Set.Set a) where
    fromJSON = fmap Set.fromList . fromJSON
    {-# INLINE fromJSON #-}

instance (ToJSON v) => ToJSON (M.Map Text v) where
    toJSON = Object . M.map toJSON
    {-# INLINE toJSON #-}

instance (FromJSON v) => FromJSON (M.Map Text v) where
    fromJSON (Object o) = M.fromAscList <$> go (M.toAscList o)
      where
        go ((k,v):kvs)  = ((:) . (,) k) <$> fromJSON v <*> go kvs
        go _            = pure []
    fromJSON _          = empty

instance (ToJSON v) => ToJSON (M.Map LT.Text v) where
    toJSON = Object . transformMap LT.toStrict toJSON

instance (FromJSON v) => FromJSON (M.Map LT.Text v) where
    fromJSON = fmap (M.mapKeysMonotonic LT.fromStrict) . fromJSON

instance (ToJSON v) => ToJSON (M.Map String v) where
    toJSON = Object . transformMap pack toJSON

instance (FromJSON v) => FromJSON (M.Map String v) where
    fromJSON = fmap (M.mapKeysMonotonic unpack) . fromJSON

instance ToJSON Value where
    toJSON a = a
    {-# INLINE toJSON #-}

instance FromJSON Value where
    fromJSON a = pure a
    {-# INLINE fromJSON #-}

-- We happen to use the same JSON formatting for a UTCTime as .NET
-- does for a DateTime. How handy!
instance ToJSON UTCTime where
    toJSON t = String (pack (formatTime defaultTimeLocale "/Date(%s)/" t))
    {-# INLINE toJSON #-}

instance FromJSON UTCTime where
    fromJSON (String t) =
        case parseTime defaultTimeLocale "/Date(%s)/" (unpack t) of
          Just d -> pure d
          _      -> empty
    fromJSON _          = empty
    {-# INLINE fromJSON #-}

instance (ToJSON a, ToJSON b) => ToJSON (a,b) where
    toJSON (a,b) = toJSON [toJSON a, toJSON b]
    {-# INLINE toJSON #-}

instance (FromJSON a, FromJSON b) => FromJSON (a,b) where
    fromJSON (Array ab) = case V.toList ab of
                            [a,b] -> (,) <$> fromJSON a <*> fromJSON b
                            _     -> empty
    fromJSON _          = empty
    {-# INLINE fromJSON #-}

instance ToJSON a => ToJSON (Dual a) where
    toJSON = toJSON . getDual
    {-# INLINE toJSON #-}

instance FromJSON a => FromJSON (Dual a) where
    fromJSON = fmap Dual . fromJSON
    {-# INLINE fromJSON #-}

instance ToJSON a => ToJSON (First a) where
    toJSON = toJSON . getFirst
    {-# INLINE toJSON #-}

instance FromJSON a => FromJSON (First a) where
    fromJSON = fmap First . fromJSON
    {-# INLINE fromJSON #-}

instance ToJSON a => ToJSON (Last a) where
    toJSON = toJSON . getLast
    {-# INLINE toJSON #-}

instance FromJSON a => FromJSON (Last a) where
    fromJSON = fmap Last . fromJSON
    {-# INLINE fromJSON #-}

-- | Transform one map into another.  The ordering of keys must be
-- preserved.
transformMap :: (Ord k1, Ord k2) => (k1 -> k2) -> (v1 -> v2)
             -> M.Map k1 v1 -> M.Map k2 v2
transformMap fk fv = M.fromAscList . map (fk *** fv) . M.toAscList

mapA :: (Alternative m) => (t -> m a) -> [t] -> m [a]
mapA f = go
  where
    go (a:as) = (:) <$> f a <*> go as
    go _      = pure []
