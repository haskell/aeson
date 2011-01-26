{-# LANGUAGE DeriveDataTypeable #-}

module Data.Aeson.Types
    (
      Array
    , Object
    , (.=)
    , (.:)
    , object
    , Value(..)
    , FromJSON(..)
    , ToJSON(..)
    ) where

import Control.Applicative
import Data.Map (Map)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, parseTime)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import System.Locale (defaultTimeLocale)
import qualified Data.Map as M
import qualified Data.Vector as V

type Object = Map Text Value
type Array = Vector Value

data Value = Object Object
           | Array Array
           | String Text
           | Number Double
           | Bool !Bool
           | Null
             deriving (Eq, Show, Typeable)

(.=) :: ToJSON a => Text -> a -> Object
name .= value = M.singleton name (toJSON value)
{-# INLINE (.=) #-}

(.:) :: (Alternative f, FromJSON a) => Object -> Text -> f a
obj .: key = case M.lookup key obj of
               Nothing -> empty
               Just v  -> fromJSON v
{-# INLINE (.:) #-}

object :: [Object] -> Value
object = Object . M.unions
{-# INLINE object #-}

class ToJSON a where
    toJSON   :: a -> Value

class FromJSON a where
    fromJSON :: Alternative f => Value -> f a

instance (ToJSON a) => ToJSON (Maybe a) where
    toJSON (Just a) = toJSON a
    toJSON Nothing  = Null
    {-# INLINE toJSON #-}
    
instance (FromJSON a) => FromJSON (Maybe a) where
    fromJSON Null   = pure Nothing
    fromJSON a      = Just <$> fromJSON a
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

instance ToJSON Text where
    toJSON = String
    {-# INLINE toJSON #-}

instance FromJSON Text where
    fromJSON (String t) = pure t
    fromJSON _          = empty
    {-# INLINE fromJSON #-}

mapA :: (Applicative f) => (t -> f a) -> [t] -> f [a]
mapA f = go
  where
    go (a:as) = (:) <$> f a <*> go as
    go []     = pure []

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










