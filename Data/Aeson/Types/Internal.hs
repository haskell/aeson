{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GeneralizedNewtypeDeriving,
    IncoherentInstances, OverlappingInstances, OverloadedStrings, Rank2Types,
    ViewPatterns, FlexibleContexts, UndecidableInstances,
    ScopedTypeVariables, PatternGuards #-}

{-# LANGUAGE CPP #-}
#ifdef GENERICS
{-# LANGUAGE DefaultSignatures
           , TypeOperators
           , EmptyDataDecls
           , KindSignatures
           , MultiParamTypeClasses
           , FunctionalDependencies
  #-}
#endif

-- |
-- Module:      Data.Aeson.Types.Internal
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types.Internal
    (
    -- * Core JSON types
      Value(..)
    , Array
    , emptyArray, isEmptyArray
    , Pair
    , Object
    , emptyObject
    -- * Convenience types and functions
    , DotNetTime(..)
    , typeMismatch
    -- * Type conversion
    , Parser
    , Result(..)
    , FromJSON(..)
    , fromJSON
    , parse
    , parseEither
    , parseMaybe
    , ToJSON(..)
    -- * Constructors and accessors
    , (.=)
    , (.:)
    , (.:?)
    , object
    ) where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Monad.State.Strict
import Data.Aeson.Functions
import Data.Attoparsec.Char8 (Number(..))
import Data.Data (Data)
import Data.Hashable (Hashable(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (foldl')
import Data.Map (Map)
import Data.Monoid (Dual(..), First(..), Last(..))
import Data.Monoid (Monoid(..))
import Data.Ratio (Ratio)
import Data.String (IsString(..))
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (FormatTime, formatTime, parseTime)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Foreign.Storable (Storable)
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HashSet
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG

#ifdef GENERICS
import GHC.Generics
#endif

-- | The result of running a 'Parser'.
data Result a = Error String
              | Success a
                deriving (Eq, Show, Typeable)

instance (NFData a) => NFData (Result a) where
    rnf (Success a) = rnf a
    rnf (Error err) = rnf err

instance Functor Result where
    fmap f (Success a) = Success (f a)
    fmap _ (Error err) = Error err
    {-# INLINE fmap #-}

instance Monad Result where
    return = Success
    {-# INLINE return #-}
    Success a >>= k = k a
    Error err >>= _ = Error err
    {-# INLINE (>>=) #-}

instance Applicative Result where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance MonadPlus Result where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a@(Success _) _ = a
    mplus _ b             = b
    {-# INLINE mplus #-}

instance Alternative Result where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance Monoid (Result a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

-- | Failure continuation.
type Failure f r   = String -> f r
-- | Success continuation.
type Success a f r = a -> f r

-- | A continuation-based parser type.
newtype Parser a = Parser {
      runParser :: forall f r.
                   Failure f r
                -> Success a f r
                -> f r
    }

instance Monad Parser where
    m >>= g = Parser $ \kf ks -> let ks' a = runParser (g a) kf ks
                                 in runParser m kf ks'
    {-# INLINE (>>=) #-}
    return a = Parser $ \_kf ks -> ks a
    {-# INLINE return #-}
    fail msg = Parser $ \kf _ks -> kf msg
    {-# INLINE fail #-}

instance Functor Parser where
    fmap f m = Parser $ \kf ks -> let ks' a = ks (f a)
                                  in runParser m kf ks'
    {-# INLINE fmap #-}

instance Applicative Parser where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = apP
    {-# INLINE (<*>) #-}

instance Alternative Parser where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance MonadPlus Parser where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a b = Parser $ \kf ks -> let kf' _ = runParser b kf ks
                                   in runParser a kf' ks
    {-# INLINE mplus #-}

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}

-- | A JSON \"object\" (key\/value map).
type Object = Map Text Value

-- | A JSON \"array\" (sequence).
type Array = Vector Value

-- | A JSON value represented as a Haskell value.
data Value = Object Object
           | Array Array
           | String Text
           | Number Number
           | Bool !Bool
           | Null
             deriving (Eq, Show, Typeable, Data)

instance NFData Value where
    rnf (Object o) = obj_rnf o
    rnf (Array a)  = V.foldl' (\x y -> rnf y `seq` x) () a
    rnf (String s) = rnf s
    rnf (Number n) = case n of I i -> rnf i; D d -> rnf d
    rnf (Bool b)   = rnf b
    rnf Null       = ()

obj_rnf :: (NFData k, NFData v) => Map k v -> ()
#if MIN_VERSION_containers(0,4,2)
obj_rnf = rnf
#elif MIN_VERSION_containers(0,4,1)
obj_rnf = M.foldlWithKey' (\_ k v -> rnf k `seq` rnf v) ()
#else
obj_rnf = rnf . M.toList
#endif

instance IsString Value where
    fromString = String . pack
    {-# INLINE fromString #-}

instance Hashable Value where
    hash (Object o) = foldl' hashWithSalt 0 . M.toList $ o
    hash (Array a)  = V.foldl' hashWithSalt 1 a
    hash (String s) = 2 `hashWithSalt` s
    hash (Number n) = 3 `hashWithSalt` case n of I i -> hash i; D d -> hash d
    hash (Bool b)   = 4 `hashWithSalt` b
    hash Null       = 5

-- | The empty array.
emptyArray :: Value
emptyArray = Array V.empty

-- | Determines if the 'Value' is an empty 'Array'.
-- Note that: @isEmptyArray 'emptyArray'@.
isEmptyArray :: Value -> Bool
isEmptyArray (Array arr) = V.null arr
isEmptyArray _ = False

-- | The empty object.
emptyObject :: Value
emptyObject = Object M.empty

-- | A key\/value pair for an 'Object'.
type Pair = (Text, Value)

-- | Construct a 'Pair' from a key and a value.
(.=) :: ToJSON a => Text -> a -> Pair
name .= value = (name, toJSON value)
{-# INLINE (.=) #-}

-- | Convert a value from JSON, failing if the types do not match.
fromJSON :: (FromJSON a) => Value -> Result a
fromJSON = parse parseJSON
{-# INLINE fromJSON #-}

-- | Run a 'Parser'.
parse :: (a -> Parser b) -> a -> Result b
parse m v = runParser (m v) Error Success
{-# INLINE parse #-}

-- | Run a 'Parser' with a 'Maybe' result type.
parseMaybe :: (a -> Parser b) -> a -> Maybe b
parseMaybe m v = runParser (m v) (const Nothing) Just
{-# INLINE parseMaybe #-}

-- | Run a 'Parser' with an 'Either' result type.
parseEither :: (a -> Parser b) -> a -> Either String b
parseEither m v = runParser (m v) Left Right
{-# INLINE parseEither #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '(.:?)' instead.
(.:) :: (FromJSON a) => Object -> Text -> Parser a
obj .: key = case M.lookup key obj of
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
obj .:? key = case M.lookup key obj of
               Nothing -> pure Nothing
               Just v  -> parseJSON v
{-# INLINE (.:?) #-}

-- | Create a 'Value' from a list of name\/value 'Pair's.  If duplicate
-- keys arise, earlier keys and their associated values win.
object :: [Pair] -> Value
object = Object . M.fromList
{-# INLINE object #-}

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
-- * 'Data.Aeson.TH' provides template-haskell functions which will derive an
-- instance at compile-time. The generated instance is optimized for your type
-- so will probably be more efficient than the following two options:
--
-- * 'Data.Aeson.Generic' provides a generic @toJSON@ function that accepts any
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
-- When writing an instance, use 'mzero' or 'fail' to make a
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
-- * 'Data.Aeson.TH' provides template-haskell functions which will derive an
-- instance at compile-time. The generated instance is optimized for your type
-- so will probably be more efficient than the following two options:
--
-- * 'Data.Aeson.Generic' provides a generic @fromJSON@ function that parses to
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
    parseJSON (Object (M.toList -> [(key, value)]))
        | key == left  = Left  <$> parseJSON value
        | key == right = Right <$> parseJSON value
    parseJSON _ = mzero
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
    toJSON = Object . M.map toJSON
    {-# INLINE toJSON #-}

instance (FromJSON v) => FromJSON (M.Map Text v) where
    parseJSON (Object o) = M.fromAscList <$> mapM go (M.toAscList o)
      where go (k,v)     = ((,) k) <$> parseJSON v
    parseJSON v          = typeMismatch "Map Text a" v

instance (ToJSON v) => ToJSON (M.Map LT.Text v) where
    toJSON = Object . transformMap LT.toStrict toJSON

instance (FromJSON v) => FromJSON (M.Map LT.Text v) where
    parseJSON = fmap (M.mapKeysMonotonic LT.fromStrict) . parseJSON

instance (ToJSON v) => ToJSON (M.Map String v) where
    toJSON = Object . transformMap pack toJSON

instance (FromJSON v) => FromJSON (M.Map String v) where
    parseJSON = fmap (M.mapKeysMonotonic unpack) . parseJSON

instance (ToJSON v) => ToJSON (M.Map B.ByteString v) where
    toJSON = Object . transformMap decode toJSON

instance (FromJSON v) => FromJSON (M.Map B.ByteString v) where
    parseJSON = fmap (M.mapKeysMonotonic encodeUtf8) . parseJSON

instance (ToJSON v) => ToJSON (M.Map LB.ByteString v) where
    toJSON = Object . transformMap strict toJSON

instance (FromJSON v) => FromJSON (M.Map LB.ByteString v) where
    parseJSON = fmap (M.mapKeysMonotonic lazy) . parseJSON

instance (ToJSON v) => ToJSON (H.HashMap Text v) where
    toJSON = Object . hashMap id toJSON
    {-# INLINE toJSON #-}

instance (FromJSON v) => FromJSON (H.HashMap Text v) where
    parseJSON (Object o) = H.fromList <$> mapM go (M.toList o)
      where go (k,v)     = ((,) k) <$> parseJSON v
    parseJSON v          = typeMismatch "HashMap Text a" v

instance (ToJSON v) => ToJSON (H.HashMap LT.Text v) where
    toJSON = Object . M.fromList . H.foldrWithKey (\k v -> ((LT.toStrict k,toJSON v) :)) []

instance (FromJSON v) => FromJSON (H.HashMap LT.Text v) where
    parseJSON = fmap (mapHash LT.fromStrict) . parseJSON

instance (ToJSON v) => ToJSON (H.HashMap String v) where
    toJSON = Object . hashMap pack toJSON

instance (FromJSON v) => FromJSON (H.HashMap String v) where
    parseJSON = fmap (mapHash unpack) . parseJSON

instance (ToJSON v) => ToJSON (H.HashMap B.ByteString v) where
    toJSON = Object . hashMap decode toJSON

instance (FromJSON v) => FromJSON (H.HashMap B.ByteString v) where
    parseJSON = fmap (mapHash encodeUtf8) . parseJSON

instance (ToJSON v) => ToJSON (H.HashMap LB.ByteString v) where
    toJSON = Object . hashMap strict toJSON

instance (FromJSON v) => FromJSON (H.HashMap LB.ByteString v) where
    parseJSON = fmap (mapHash lazy) . parseJSON

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
    toJSON (a,b) = toJSON [toJSON a, toJSON b]
    {-# INLINE toJSON #-}

instance (FromJSON a, FromJSON b) => FromJSON (a,b) where
    parseJSON (Array ab) =
      case V.toList ab of
        [a,b] -> (,) <$> parseJSON a <*> parseJSON b
        _     -> fail $ "cannot unpack array of length " ++
                        show (V.length ab) ++ " into a pair"
    parseJSON v          = typeMismatch "(a,b)" v
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a,b,c) where
    toJSON (a,b,c) = toJSON [toJSON a, toJSON b, toJSON c]
    {-# INLINE toJSON #-}

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (a,b,c) where
    parseJSON (Array abc) =
      case V.toList abc of
        [a,b,c] -> (,,) <$> parseJSON a <*> parseJSON b <*> parseJSON c
        _       -> fail $ "cannot unpack array of length " ++
                          show (V.length abc) ++ " into a 3-tuple"
    parseJSON v          = typeMismatch "(a,b,c)" v
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

#ifdef GENERICS
--------------------------------------------------------------------------------
-- Generic toJSON

class GToJSON f where
    gToJSON :: f a -> Value

instance (GToJSON a) => GToJSON (M1 i c a) where
    gToJSON = gToJSON . unM1
    {-# INLINE gToJSON #-}

instance (ToJSON a) => GToJSON (K1 i a) where
    gToJSON = toJSON . unK1
    {-# INLINE gToJSON #-}

instance GToJSON U1 where
    gToJSON _ = emptyArray
    {-# INLINE gToJSON #-}

instance (ConsToJSON a) => GToJSON (C1 c a) where
    gToJSON = consToJSON . unM1
    {-# INLINE gToJSON #-}

instance (GProductToValues a, GProductToValues b) => GToJSON (a :*: b) where
    gToJSON = toJSON . toList . gProductToValues
    {-# INLINE gToJSON #-}

instance (GObject a, GObject b) => GToJSON (a :+: b) where
    gToJSON (L1 x) = Object $ gObject x
    gToJSON (R1 x) = Object $ gObject x
    {-# INLINE gToJSON #-}

--------------------------------------------------------------------------------

class ConsToJSON    f where consToJSON  ::           f a -> Value
class ConsToJSON' b f where consToJSON' :: Tagged b (f a -> Value)

newtype Tagged s b = Tagged {unTagged :: b}

instance (IsRecord f b, ConsToJSON' b f) => ConsToJSON f where
    consToJSON = unTagged (consToJSON' :: Tagged b (f a -> Value))
    {-# INLINE consToJSON #-}

instance (GRecordToPairs f) => ConsToJSON' True f where
    consToJSON' = Tagged (object . toList . gRecordToPairs)
    {-# INLINE consToJSON' #-}

instance GToJSON f => ConsToJSON' False f where
    consToJSON' = Tagged gToJSON
    {-# INLINE consToJSON' #-}

--------------------------------------------------------------------------------

class GRecordToPairs f where
    gRecordToPairs :: f a -> DList Pair

instance (GRecordToPairs a, GRecordToPairs b) => GRecordToPairs (a :*: b) where
    gRecordToPairs (a :*: b) = gRecordToPairs a `append` gRecordToPairs b
    {-# INLINE gRecordToPairs #-}

instance (Selector s, GToJSON a) => GRecordToPairs (S1 s a) where
    gRecordToPairs m1 = singleton (pack (selName m1), gToJSON (unM1 m1))
    {-# INLINE gRecordToPairs #-}

--------------------------------------------------------------------------------

class GProductToValues f where
    gProductToValues :: f a -> DList Value

instance (GProductToValues a, GProductToValues b) => GProductToValues (a :*: b) where
    gProductToValues (a :*: b) = gProductToValues a `append` gProductToValues b
    {-# INLINE gProductToValues #-}

instance (GToJSON a) => GProductToValues a where
    gProductToValues = singleton . gToJSON
    {-# INLINE gProductToValues #-}

--------------------------------------------------------------------------------

class GObject f where
    gObject :: f a -> Object

instance (GObject a, GObject b) => GObject (a :+: b) where
    gObject (L1 x) = gObject x
    gObject (R1 x) = gObject x
    {-# INLINE gObject #-}

instance (Constructor c, GToJSON a, ConsToJSON a) => GObject (C1 c a) where
    gObject m1 = M.singleton (pack (conName m1)) (gToJSON m1)
    {-# INLINE gObject #-}

--------------------------------------------------------------------------------
-- Generic parseJSON

class GFromJSON f where
    gParseJSON :: Value -> Parser (f a)

instance (GFromJSON a) => GFromJSON (M1 i c a) where
    gParseJSON = fmap M1 . gParseJSON
    {-# INLINE gParseJSON #-}

instance (FromJSON a) => GFromJSON (K1 i a) where
    gParseJSON = fmap K1 . parseJSON
    {-# INLINE gParseJSON #-}

instance GFromJSON U1 where
    gParseJSON v
        | isEmptyArray v = pure U1
        | otherwise      = typeMismatch "unit constructor (U1)" v
    {-# INLINE gParseJSON #-}

instance (ConsFromJSON a) => GFromJSON (C1 c a) where
    gParseJSON = fmap M1 . consParseJSON
    {-# INLINE gParseJSON #-}

instance ( GFromProduct a, GFromProduct b
         , ProductSize a, ProductSize b) => GFromJSON (a :*: b) where
    gParseJSON (Array arr)
        | lenArray == lenProduct = gParseProduct arr
        | otherwise =
            fail $ "When expecting a product of " ++ show lenProduct ++
                   " values, encountered an Array of " ++ show lenArray ++
                   " elements instead"
        where
          lenArray = V.length arr
          lenProduct = unTagged2 (productSize :: Tagged2 (a :*: b) Int)

    gParseJSON v = typeMismatch "product (:*:)" v
    {-# INLINE gParseJSON #-}

instance (GFromSum a, GFromSum b) => GFromJSON (a :+: b) where
    gParseJSON (Object (M.toList -> [keyVal])) = gParseSum keyVal
    gParseJSON v = typeMismatch "sum (:+:)" v
    {-# INLINE gParseJSON #-}

--------------------------------------------------------------------------------

class ConsFromJSON    f where consParseJSON  ::           Value -> Parser (f a)
class ConsFromJSON' b f where consParseJSON' :: Tagged b (Value -> Parser (f a))

instance (IsRecord f b, ConsFromJSON' b f) => ConsFromJSON f where
    consParseJSON = unTagged (consParseJSON' :: Tagged b (Value -> Parser (f a)))
    {-# INLINE consParseJSON #-}

instance (GFromRecord f) => ConsFromJSON' True f where
    consParseJSON' = Tagged parseRecord
        where
          parseRecord (Object obj) = gParseRecord obj
          parseRecord v = typeMismatch "record (:*:)" v
    {-# INLINE consParseJSON' #-}

instance (GFromJSON f) => ConsFromJSON' False f where
    consParseJSON' = Tagged gParseJSON
    {-# INLINE consParseJSON' #-}

--------------------------------------------------------------------------------

class GFromRecord f where
    gParseRecord :: Object -> Parser (f a)

instance (GFromRecord a, GFromRecord b) => GFromRecord (a :*: b) where
    gParseRecord obj = (:*:) <$> gParseRecord obj <*> gParseRecord obj
    {-# INLINE gParseRecord #-}

instance (Selector s, GFromJSON a) => GFromRecord (S1 s a) where
    gParseRecord obj = case M.lookup (T.pack key) obj of
                         Nothing -> notFound key
                         Just v  -> gParseJSON v
        where
          key = selName (undefined :: t s a p)
    {-# INLINE gParseRecord #-}

--------------------------------------------------------------------------------

class ProductSize f where
    productSize :: Tagged2 f Int

newtype Tagged2 (s :: * -> *) b = Tagged2 {unTagged2 :: b}

instance (ProductSize a, ProductSize b) => ProductSize (a :*: b) where
    productSize = Tagged2 $ unTagged2 (productSize :: Tagged2 a Int) +
                            unTagged2 (productSize :: Tagged2 b Int)

instance ProductSize (S1 s a) where
    productSize = Tagged2 1

--------------------------------------------------------------------------------

class GFromProduct f where
    gParseProduct :: Array -> Parser (f a)

instance (GFromProduct a, GFromProduct b) => GFromProduct (a :*: b) where
    gParseProduct arr = (:*:) <$> gParseProduct arrL <*> gParseProduct arrR
        where
          (arrL, arrR) = V.splitAt (V.length arr `div` 2) arr
    {-# INLINE gParseProduct #-}

instance (GFromJSON a) => GFromProduct (S1 s a) where
    gParseProduct arr = gParseJSON $ V.unsafeIndex arr 0
    {-# INLINE gParseProduct #-}

--------------------------------------------------------------------------------

class GFromSum f where
    gParseSum :: Pair -> Parser (f a)

instance (GFromSum a, GFromSum b) => GFromSum (a :+: b) where
    gParseSum keyVal = (L1 <$> gParseSum keyVal) <|> (R1 <$> gParseSum keyVal)
    {-# INLINE gParseSum #-}

instance (Constructor c, GFromJSON a, ConsFromJSON a) => GFromSum (C1 c a) where
    gParseSum (key, value)
        | key == pack (conName (undefined :: t c a p)) = gParseJSON value
        | otherwise = notFound $ unpack key
    {-# INLINE gParseSum #-}

notFound :: String -> Parser a
notFound key = fail $ "The key \"" ++ key ++ "\" was not found"
{-# INLINE notFound #-}

--------------------------------------------------------------------------------

class IsRecord (f :: * -> *) b | f -> b

data True
data False

instance (IsRecord f b) => IsRecord (f :*: g) b
instance IsRecord (M1 S NoSelector f) False
instance (IsRecord f b) => IsRecord (M1 S c f) b
instance IsRecord (K1 i c) True
instance IsRecord U1 False

--------------------------------------------------------------------------------

type DList a = [a] -> [a]

toList :: DList a -> [a]
toList = ($ [])
{-# INLINE toList #-}

singleton :: a -> DList a
singleton = (:)
{-# INLINE singleton #-}

append :: DList a -> DList a -> DList a
append = (.)
{-# INLINE append #-}

--------------------------------------------------------------------------------
#endif
