{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
    GeneralizedNewtypeDeriving, IncoherentInstances, OverlappingInstances,
    OverloadedStrings, UndecidableInstances, ViewPatterns #-}

#ifdef GENERICS
{-# LANGUAGE DefaultSignatures #-}
#endif

-- |
-- Module:      Data.Aeson.Types.Class
-- Copyright:   (c) 2011 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types.Class
    (
    -- * Type classes
    -- ** Core JSON classes
      FromJSON(..)
    , ToJSON(..)
#ifdef GENERICS
    -- ** Generic JSON classes
    , GFromJSON(..)
    , GToJSON(..)
#endif
    -- * Types
    , DotNetTime(..)
    -- * Functions
    , fromJSON
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    , typeMismatch
    ) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Aeson.Functions
import Data.Aeson.Types.Internal
import Data.Attoparsec.Char8 (Number(..))
import Data.Hashable (Hashable(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Monoid (Dual(..), First(..), Last(..))
import Data.Ratio (Ratio)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (FormatTime, formatTime, parseTime)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Foreign.Storable (Storable)
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Mutable as VM ( unsafeNew, unsafeWrite )

#ifdef GENERICS
import GHC.Generics

class GToJSON f where
    gToJSON :: f a -> Value

class GFromJSON f where
    gParseJSON :: Value -> Parser (f a)
#endif

-- | A type that can be converted to JSON.
--
-- An example type and instance:
--
-- @{-\# LANGUAGE OverloadedStrings #-}
--
-- data Coord { x :: Double, y :: Double }
--
-- instance ToJSON Coord where
--   toJSON (Coord x y) = 'object' [\"x\" '.=' x, \"y\" '.=' y]
-- @
--
-- Note the use of the @OverloadedStrings@ language extension which enables
-- 'Text' values to be written as string literals.
--
-- Instead of manually writing your 'ToJSON' instance, there are three options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides template-haskell functions which will derive an
-- instance at compile-time. The generated instance is optimized for your type
-- so will probably be more efficient than the following two options:
--
-- * "Data.Aeson.Generic" provides a generic @toJSON@ function that accepts any
-- type which is an instance of 'Data'.
--
-- * If your compiler has support for the @DeriveGeneric@ and
-- @DefaultSignatures@ language extensions, @toJSON@ will have a default generic
-- implementation.
--
-- To use the latter option, simply add a @deriving 'Generic'@ clause to your
-- datatype and declare a @ToJSON@ instance for your datatype without giving a
-- definition for @toJSON@.
--
-- For example the previous example can be simplified to just:
--
-- @{-\# LANGUAGE DeriveGeneric \#-}
--
-- import GHC.Generics
--
-- data Coord { x :: Double, y :: Double } deriving Generic
--
-- instance ToJSON Coord
-- @
class ToJSON a where
    toJSON   :: a -> Value

#ifdef GENERICS
    default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
    toJSON = gToJSON . from
#endif

-- | A type that can be converted from JSON, with the possibility of
-- failure.
--
-- When writing an instance, use 'empty', 'mzero', or 'fail' to make a
-- conversion fail, e.g. if an 'Object' is missing a required key, or
-- the value is of the wrong type.
--
-- An example type and instance:
--
-- @{-\# LANGUAGE OverloadedStrings #-}
--
-- data Coord { x :: Double, y :: Double }
--
-- instance FromJSON Coord where
--   parseJSON ('Object' v) = Coord    '<$>'
--                          v '.:' \"x\" '<*>'
--                          v '.:' \"y\"
--
--   \-- A non-'Object' value is of the wrong type, so use 'mzero' to fail.
--   parseJSON _          = 'mzero'
-- @
--
-- Note the use of the @OverloadedStrings@ language extension which enables
-- 'Text' values to be written as string literals.
--
-- Instead of manually writing your 'FromJSON' instance, there are three options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides template-haskell functions which will derive an
-- instance at compile-time. The generated instance is optimized for your type
-- so will probably be more efficient than the following two options:
--
-- * "Data.Aeson.Generic" provides a generic @fromJSON@ function that parses to
-- any type which is an instance of 'Data'.
--
-- * If your compiler has support for the @DeriveGeneric@ and
-- @DefaultSignatures@ language extensions, @parseJSON@ will have a default
-- generic implementation.
--
-- To use this, simply add a @deriving 'Generic'@ clause to your datatype and
-- declare a @FromJSON@ instance for your datatype without giving a definition
-- for @parseJSON@.
--
-- For example the previous example can be simplified to just:
--
-- @{-\# LANGUAGE DeriveGeneric \#-}
--
-- import GHC.Generics
--
-- data Coord { x :: Double, y :: Double } deriving Generic
--
-- instance FromJSON Coord
-- @
class FromJSON a where
    parseJSON :: Value -> Parser a

#ifdef GENERICS
    default parseJSON :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
    parseJSON = fmap to . gParseJSON
#endif

instance (ToJSON a) => ToJSON (Maybe a) where
    toJSON (Just a) = toJSON a
    toJSON Nothing  = Null
    {-# INLINE toJSON #-}

instance (FromJSON a) => FromJSON (Maybe a) where
    parseJSON Null   = pure Nothing
    parseJSON a      = Just <$> parseJSON a
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b) => ToJSON (Either a b) where
    toJSON (Left a)  = object [left  .= a]
    toJSON (Right b) = object [right .= b]
    {-# INLINE toJSON #-}

instance (FromJSON a, FromJSON b) => FromJSON (Either a b) where
    parseJSON (Object (H.toList -> [(key, value)]))
        | key == left  = Left  <$> parseJSON value
        | key == right = Right <$> parseJSON value
    parseJSON _        = fail ""
    {-# INLINE parseJSON #-}

left, right :: Text
left  = "Left"
right = "Right"

instance ToJSON Bool where
    toJSON = Bool
    {-# INLINE toJSON #-}

instance FromJSON Bool where
    parseJSON (Bool b) = pure b
    parseJSON v        = typeMismatch "Bool" v
    {-# INLINE parseJSON #-}

instance ToJSON () where
    toJSON _ = emptyArray
    {-# INLINE toJSON #-}

instance FromJSON () where
    parseJSON (Array v) | V.null v = pure ()
    parseJSON v        = typeMismatch "()" v
    {-# INLINE parseJSON #-}

instance ToJSON [Char] where
    toJSON = String . T.pack
    {-# INLINE toJSON #-}

instance FromJSON [Char] where
    parseJSON (String t) = pure (T.unpack t)
    parseJSON v          = typeMismatch "String" v
    {-# INLINE parseJSON #-}

instance ToJSON Char where
    toJSON = String . T.singleton
    {-# INLINE toJSON #-}

instance FromJSON Char where
    parseJSON (String t)
        | T.compareLength t 1 == EQ = pure (T.head t)
    parseJSON v          = typeMismatch "Char" v
    {-# INLINE parseJSON #-}

instance ToJSON Double where
    toJSON = Number . D
    {-# INLINE toJSON #-}

instance FromJSON Double where
    parseJSON (Number n) = case n of
                             D d -> pure d
                             I i -> pure (fromIntegral i)
    parseJSON Null       = pure (0/0)
    parseJSON v          = typeMismatch "Double" v
    {-# INLINE parseJSON #-}

instance ToJSON Number where
    toJSON = Number
    {-# INLINE toJSON #-}

instance FromJSON Number where
    parseJSON (Number n) = pure n
    parseJSON Null       = pure (D (0/0))
    parseJSON v          = typeMismatch "Number" v
    {-# INLINE parseJSON #-}

instance ToJSON Float where
    toJSON = Number . realToFrac
    {-# INLINE toJSON #-}

instance FromJSON Float where
    parseJSON (Number n) = pure $ case n of
                                    D d -> realToFrac d
                                    I i -> fromIntegral i
    parseJSON Null       = pure (0/0)
    parseJSON v          = typeMismatch "Float" v
    {-# INLINE parseJSON #-}

instance ToJSON (Ratio Integer) where
    toJSON = Number . fromRational
    {-# INLINE toJSON #-}

instance FromJSON (Ratio Integer) where
    parseJSON (Number n) = pure $ case n of
                                    D d -> toRational d
                                    I i -> fromIntegral i
    parseJSON v          = typeMismatch "Ratio Integer" v
    {-# INLINE parseJSON #-}

instance ToJSON Int where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Int where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

parseIntegral :: Integral a => Value -> Parser a
parseIntegral (Number n) = pure (floor n)
parseIntegral v          = typeMismatch "Integral" v
{-# INLINE parseIntegral #-}

instance ToJSON Integer where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Integer where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Int8 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Int8 where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Int16 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Int16 where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Int32 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Int32 where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Int64 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Int64 where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Word where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Word where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Word8 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Word8 where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Word16 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Word16 where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Word32 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Word32 where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Word64 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON Word64 where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

instance ToJSON Text where
    toJSON = String
    {-# INLINE toJSON #-}

instance FromJSON Text where
    parseJSON (String t) = pure t
    parseJSON v          = typeMismatch "Text" v
    {-# INLINE parseJSON #-}

instance ToJSON LT.Text where
    toJSON = String . LT.toStrict
    {-# INLINE toJSON #-}

instance FromJSON LT.Text where
    parseJSON (String t) = pure (LT.fromStrict t)
    parseJSON v          = typeMismatch "Lazy Text" v
    {-# INLINE parseJSON #-}

instance ToJSON B.ByteString where
    toJSON = String . decode
    {-# INLINE toJSON #-}

instance FromJSON B.ByteString where
    parseJSON (String t) = pure . encodeUtf8 $ t
    parseJSON v          = typeMismatch "ByteString" v
    {-# INLINE parseJSON #-}

instance ToJSON LB.ByteString where
    toJSON = toJSON . strict
    {-# INLINE toJSON #-}

instance FromJSON LB.ByteString where
    parseJSON (String t) = pure . lazy $ t
    parseJSON v          = typeMismatch "Lazy ByteString" v
    {-# INLINE parseJSON #-}

instance (ToJSON a) => ToJSON [a] where
    toJSON = Array . V.fromList . map toJSON
    {-# INLINE toJSON #-}

instance (FromJSON a) => FromJSON [a] where
    parseJSON (Array a) = mapM parseJSON (V.toList a)
    parseJSON v         = typeMismatch "[a]" v
    {-# INLINE parseJSON #-}

instance (ToJSON a) => ToJSON (Vector a) where
    toJSON = Array . V.map toJSON
    {-# INLINE toJSON #-}

instance (FromJSON a) => FromJSON (Vector a) where
    parseJSON (Array a) = V.mapM parseJSON a
    parseJSON v         = typeMismatch "Vector a" v
    {-# INLINE parseJSON #-}

vectorToJSON :: (VG.Vector v a, ToJSON a) => v a -> Value
vectorToJSON = Array . V.map toJSON . V.convert
{-# INLINE vectorToJSON #-}

vectorParseJSON :: (FromJSON a, VG.Vector w a) => String -> Value -> Parser (w a)
vectorParseJSON _ (Array a) = V.convert <$> V.mapM parseJSON a
vectorParseJSON s v         = typeMismatch s v
{-# INLINE vectorParseJSON #-}

instance (Storable a, ToJSON a) => ToJSON (VS.Vector a) where
    toJSON = vectorToJSON

instance (Storable a, FromJSON a) => FromJSON (VS.Vector a) where
    parseJSON = vectorParseJSON "Data.Vector.Storable.Vector a"

instance (VP.Prim a, ToJSON a) => ToJSON (VP.Vector a) where
    toJSON = vectorToJSON

instance (VP.Prim a, FromJSON a) => FromJSON (VP.Vector a) where
    parseJSON = vectorParseJSON "Data.Vector.Primitive.Vector a"

instance (VG.Vector VU.Vector a, ToJSON a) => ToJSON (VU.Vector a) where
    toJSON = vectorToJSON

instance (VG.Vector VU.Vector a, FromJSON a) => FromJSON (VU.Vector a) where
    parseJSON = vectorParseJSON "Data.Vector.Unboxed.Vector a"

instance (ToJSON a) => ToJSON (Set.Set a) where
    toJSON = toJSON . Set.toList
    {-# INLINE toJSON #-}

instance (Ord a, FromJSON a) => FromJSON (Set.Set a) where
    parseJSON = fmap Set.fromList . parseJSON
    {-# INLINE parseJSON #-}

instance (ToJSON a) => ToJSON (HashSet.HashSet a) where
    toJSON = toJSON . HashSet.toList
    {-# INLINE toJSON #-}

instance (Eq a, Hashable a, FromJSON a) => FromJSON (HashSet.HashSet a) where
    parseJSON = fmap HashSet.fromList . parseJSON
    {-# INLINE parseJSON #-}

instance ToJSON IntSet.IntSet where
    toJSON = toJSON . IntSet.toList
    {-# INLINE toJSON #-}

instance FromJSON IntSet.IntSet where
    parseJSON = fmap IntSet.fromList . parseJSON
    {-# INLINE parseJSON #-}

instance ToJSON a => ToJSON (IntMap.IntMap a) where
    toJSON = toJSON . IntMap.toList
    {-# INLINE toJSON #-}

instance FromJSON a => FromJSON (IntMap.IntMap a) where
    parseJSON = fmap IntMap.fromList . parseJSON
    {-# INLINE parseJSON #-}

instance (ToJSON v) => ToJSON (M.Map Text v) where
    toJSON = Object . M.foldrWithKey (\k -> H.insert k . toJSON) H.empty
    {-# INLINE toJSON #-}

instance (FromJSON v) => FromJSON (M.Map Text v) where
    parseJSON (Object o) = H.foldrWithKey M.insert M.empty <$> traverse parseJSON o
    parseJSON v          = typeMismatch "Map Text a" v

instance (ToJSON v) => ToJSON (M.Map LT.Text v) where
    toJSON = Object . mapHashKeyVal LT.toStrict toJSON

instance (FromJSON v) => FromJSON (M.Map LT.Text v) where
    parseJSON = fmap (hashMapKey LT.fromStrict) . parseJSON

instance (ToJSON v) => ToJSON (M.Map String v) where
    toJSON = Object . mapHashKeyVal pack toJSON

instance (FromJSON v) => FromJSON (M.Map String v) where
    parseJSON = fmap (hashMapKey unpack) . parseJSON

instance (ToJSON v) => ToJSON (M.Map B.ByteString v) where
    toJSON = Object . mapHashKeyVal decode toJSON

instance (FromJSON v) => FromJSON (M.Map B.ByteString v) where
    parseJSON = fmap (hashMapKey encodeUtf8) . parseJSON

instance (ToJSON v) => ToJSON (M.Map LB.ByteString v) where
    toJSON = Object . mapHashKeyVal strict toJSON

instance (FromJSON v) => FromJSON (M.Map LB.ByteString v) where
    parseJSON = fmap (hashMapKey lazy) . parseJSON

instance (ToJSON v) => ToJSON (H.HashMap Text v) where
    toJSON = Object . H.map toJSON
    {-# INLINE toJSON #-}

instance (FromJSON v) => FromJSON (H.HashMap Text v) where
    parseJSON (Object o) = traverse parseJSON o
    parseJSON v          = typeMismatch "HashMap Text a" v

instance (ToJSON v) => ToJSON (H.HashMap LT.Text v) where
    toJSON = Object . mapKeyVal LT.toStrict toJSON

instance (FromJSON v) => FromJSON (H.HashMap LT.Text v) where
    parseJSON = fmap (mapKey LT.fromStrict) . parseJSON

instance (ToJSON v) => ToJSON (H.HashMap String v) where
    toJSON = Object . mapKeyVal pack toJSON

instance (FromJSON v) => FromJSON (H.HashMap String v) where
    parseJSON = fmap (mapKey unpack) . parseJSON

instance (ToJSON v) => ToJSON (H.HashMap B.ByteString v) where
    toJSON = Object . mapKeyVal decode toJSON

instance (FromJSON v) => FromJSON (H.HashMap B.ByteString v) where
    parseJSON = fmap (mapKey encodeUtf8) . parseJSON

instance (ToJSON v) => ToJSON (H.HashMap LB.ByteString v) where
    toJSON = Object . mapKeyVal strict toJSON

instance (FromJSON v) => FromJSON (H.HashMap LB.ByteString v) where
    parseJSON = fmap (mapKey lazy) . parseJSON

instance ToJSON Value where
    toJSON a = a
    {-# INLINE toJSON #-}

instance FromJSON Value where
    parseJSON a = pure a
    {-# INLINE parseJSON #-}

-- | A newtype wrapper for 'UTCTime' that uses the same non-standard
-- serialization format as Microsoft .NET, whose @System.DateTime@
-- type is by default serialized to JSON as in the following example:
--
-- > /Date(1302547608878)/
--
-- The number represents milliseconds since the Unix epoch.
newtype DotNetTime = DotNetTime {
      fromDotNetTime :: UTCTime
    } deriving (Eq, Ord, Read, Show, Typeable, FormatTime)

instance ToJSON DotNetTime where
    toJSON (DotNetTime t) =
        String (pack (secs ++ msecs ++ ")/"))
      where secs  = formatTime defaultTimeLocale "/Date(%s" t
            msecs = take 3 $ formatTime defaultTimeLocale "%q" t
    {-# INLINE toJSON #-}

instance FromJSON DotNetTime where
    parseJSON (String t) =
        case parseTime defaultTimeLocale "/Date(%s%Q)/" (unpack t') of
          Just d -> pure (DotNetTime d)
          _      -> fail "could not parse .NET time"
      where (s,m) = T.splitAt (T.length t - 5) t
            t'    = T.concat [s,".",m]
    parseJSON v   = typeMismatch "DotNetTime" v
    {-# INLINE parseJSON #-}

instance ToJSON UTCTime where
    toJSON t = String (pack (take 23 str ++ "Z"))
      where str = formatTime defaultTimeLocale "%FT%T%Q" t
    {-# INLINE toJSON #-}

instance FromJSON UTCTime where
    parseJSON (String t) =
        case parseTime defaultTimeLocale "%FT%T%QZ" (unpack t) of
          Just d -> pure d
          _      -> fail "could not parse ISO-8601 date"
    parseJSON v   = typeMismatch "UTCTime" v
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b) => ToJSON (a,b) where
    toJSON (a,b) = Array $ V.create $ do
                     mv <- VM.unsafeNew 2
                     VM.unsafeWrite mv 0 (toJSON a)
                     VM.unsafeWrite mv 1 (toJSON b)
                     return mv
    {-# INLINE toJSON #-}

instance (FromJSON a, FromJSON b) => FromJSON (a,b) where
    parseJSON (Array ab)
        | n == 2    = (,) <$> parseJSON (V.unsafeIndex ab 0)
                          <*> parseJSON (V.unsafeIndex ab 1)
        | otherwise = fail $ "cannot unpack array of length " ++
                        show n ++ " into a pair"
          where
            n = V.length ab
    parseJSON v = typeMismatch "(a,b)" v
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a,b,c) where
    toJSON (a,b,c) = Array $ V.create $ do
                       mv <- VM.unsafeNew 3
                       VM.unsafeWrite mv 0 (toJSON a)
                       VM.unsafeWrite mv 1 (toJSON b)
                       VM.unsafeWrite mv 2 (toJSON c)
                       return mv
    {-# INLINE toJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (a,b,c) where
    parseJSON (Array abc)
        | n == 3    = (,,) <$> parseJSON (V.unsafeIndex abc 0)
                           <*> parseJSON (V.unsafeIndex abc 1)
                           <*> parseJSON (V.unsafeIndex abc 2)
        | otherwise = fail $ "cannot unpack array of length " ++
                        show n ++ " into a 3-tuple"
          where
            n = V.length abc
    parseJSON v = typeMismatch "(a,b,c)" v
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (a,b,c,d) where
    toJSON (a,b,c,d) = Array $ V.create $ do
                         mv <- VM.unsafeNew 4
                         VM.unsafeWrite mv 0 (toJSON a)
                         VM.unsafeWrite mv 1 (toJSON b)
                         VM.unsafeWrite mv 2 (toJSON c)
                         VM.unsafeWrite mv 3 (toJSON d)
                         return mv
    {-# INLINE toJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSON (a,b,c,d) where
    parseJSON (Array abcd)
        | n == 4    = (,,,) <$> parseJSON (V.unsafeIndex abcd 0)
                            <*> parseJSON (V.unsafeIndex abcd 1)
                            <*> parseJSON (V.unsafeIndex abcd 2)
                            <*> parseJSON (V.unsafeIndex abcd 3)
        | otherwise = fail $ "cannot unpack array of length " ++
                        show n ++ " into a 4-tuple"
          where
            n = V.length abcd
    parseJSON v = typeMismatch "(a,b,c,d)" v
    {-# INLINE parseJSON #-}

instance ToJSON a => ToJSON (Dual a) where
    toJSON = toJSON . getDual
    {-# INLINE toJSON #-}

instance FromJSON a => FromJSON (Dual a) where
    parseJSON = fmap Dual . parseJSON
    {-# INLINE parseJSON #-}

instance ToJSON a => ToJSON (First a) where
    toJSON = toJSON . getFirst
    {-# INLINE toJSON #-}

instance FromJSON a => FromJSON (First a) where
    parseJSON = fmap First . parseJSON
    {-# INLINE parseJSON #-}

instance ToJSON a => ToJSON (Last a) where
    toJSON = toJSON . getLast
    {-# INLINE toJSON #-}

instance FromJSON a => FromJSON (Last a) where
    parseJSON = fmap Last . parseJSON
    {-# INLINE parseJSON #-}

-- | Construct a 'Pair' from a key and a value.
(.=) :: ToJSON a => Text -> a -> Pair
name .= value = (name, toJSON value)
{-# INLINE (.=) #-}

-- | Convert a value from JSON, failing if the types do not match.
fromJSON :: (FromJSON a) => Value -> Result a
fromJSON = parse parseJSON
{-# INLINE fromJSON #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '(.:?)' instead.
(.:) :: (FromJSON a) => Object -> Text -> Parser a
obj .: key = case H.lookup key obj of
               Nothing -> fail $ "key " ++ show key ++ " not present"
               Just v  -> parseJSON v
{-# INLINE (.:) #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present, or 'empty' if
-- the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '(.:)' instead.
(.:?) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
obj .:? key = case H.lookup key obj of
               Nothing -> pure Nothing
               Just v  -> parseJSON v
{-# INLINE (.:?) #-}

-- | Helper for use in combination with '.:?' to provide default
-- values for optional JSON object fields.
--
-- This combinator is most useful if the key and value can be absent
-- from an object without affecting its validity and we know a default
-- value to assign in that case.  If the key and value are mandatory,
-- use '(.:)' instead.
-- 
-- Example usage:
--
-- @ v1 <- o '.:?' \"opt_field_with_dfl\" .!= \"default_val\"
-- v2 <- o '.:'  \"mandatory_field\"
-- v3 <- o '.:?' \"opt_field2\"
-- @
(.!=) :: Parser (Maybe a) -> a -> Parser a
pmval .!= val = fromMaybe val <$> pmval
{-# INLINE (.!=) #-}

-- | Fail parsing due to a type mismatch, with a descriptive message.
typeMismatch :: String -- ^ The name of the type you are trying to parse.
             -> Value  -- ^ The actual value encountered.
             -> Parser a
typeMismatch expected actual =
    fail $ "when expecting a " ++ expected ++ ", encountered " ++ name ++
           " instead"
  where
    name = case actual of
             Object _ -> "Object"
             Array _  -> "Array"
             String _ -> "String"
             Number _ -> "Number"
             Bool _   -> "Boolean"
             Null     -> "Null"
