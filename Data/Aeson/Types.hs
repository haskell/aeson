module Data.Aeson.Types
    (
      Array
    , Object
    , (.=)
    , (.:)
    , object
    , Value(..)
    , JSON(..)
    ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, parseTime)
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
             deriving (Eq, Show)

(.=) :: JSON a => Text -> a -> Object
name .= value = M.singleton name (toJSON value)
{-# INLINE (.=) #-}

(.:) :: (Alternative f, JSON a) => Object -> Text -> f a
obj .: key = case M.lookup key obj of
               Nothing -> empty
               Just v  -> fromJSON v
{-# INLINE (.:) #-}

object :: [Object] -> Value
object = Object . M.unions
{-# INLINE object #-}

class JSON a where
    toJSON   :: a -> Value
    fromJSON :: Alternative f => Value -> f a

instance (JSON a) => JSON (Maybe a) where
    toJSON (Just a) = toJSON a
    toJSON Nothing  = Null
    {-# INLINE toJSON #-}
    
    fromJSON Null   = pure Nothing
    fromJSON a      = Just <$> fromJSON a
    {-# INLINE fromJSON #-}

instance JSON Bool where
    toJSON = Bool
    {-# INLINE toJSON #-}

    fromJSON (Bool b) = pure b
    fromJSON _        = empty
    {-# INLINE fromJSON #-}

instance JSON Double where
    toJSON = Number
    {-# INLINE toJSON #-}

    fromJSON (Number n) = pure n
    fromJSON _          = empty
    {-# INLINE fromJSON #-}

instance JSON Int where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    fromJSON (Number n) = pure (floor n)
    fromJSON _          = empty
    {-# INLINE fromJSON #-}

instance JSON Text where
    toJSON = String
    {-# INLINE toJSON #-}

    fromJSON (String t) = pure t
    fromJSON _          = empty
    {-# INLINE fromJSON #-}

instance JSON ByteString where
    toJSON = String . decodeUtf8

    fromJSON (String t) = pure (encodeUtf8 t)
    fromJSON _          = empty

mapA :: (Applicative f) => (t -> f a) -> [t] -> f [a]
mapA f = go
  where
    go (a:as) = (:) <$> f a <*> go as
    go []     = pure []

instance (JSON a) => JSON [a] where
    toJSON = Array . V.fromList . map toJSON
    {-# INLINE toJSON #-}
    
    fromJSON (Array a) = mapA fromJSON (V.toList a)
    fromJSON _         = empty
    {-# INLINE fromJSON #-}

instance (JSON a) => JSON (Vector a) where
    toJSON = Array . V.map toJSON
    {-# INLINE toJSON #-}
    
    fromJSON (Array a) = V.fromList <$> mapA fromJSON (V.toList a)
    fromJSON _         = empty
    {-# INLINE fromJSON #-}

instance JSON Value where
    toJSON a = a
    {-# INLINE toJSON #-}

    fromJSON a = pure a
    {-# INLINE fromJSON #-}

-- We happen to use the same JSON formatting for a UTCTime as .NET
-- does for a DateTime. How handy!
instance JSON UTCTime where
    toJSON t = String (pack (formatTime defaultTimeLocale "/Date(%s)/" t))
    {-# INLINE toJSON #-}

    fromJSON (String t) =
        case parseTime defaultTimeLocale "/Date(%s)/" (unpack t) of
          Just d -> pure d
          _      -> empty
    fromJSON _          = empty
    {-# INLINE fromJSON #-}

instance (JSON a, JSON b) => JSON (a,b) where
    toJSON (a,b) = toJSON [toJSON a, toJSON b]
    {-# INLINE toJSON #-}

    fromJSON (Array ab) = case V.toList ab of
                            [a,b] -> (,) <$> fromJSON a <*> fromJSON b
                            _     -> empty
    fromJSON _          = empty
    {-# INLINE fromJSON #-}










