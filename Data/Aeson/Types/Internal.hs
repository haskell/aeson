{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GeneralizedNewtypeDeriving,
    IncoherentInstances, OverlappingInstances, OverloadedStrings, Rank2Types,
    ViewPatterns, FlexibleContexts, UndecidableInstances,
    ScopedTypeVariables, PatternGuards #-}

{-# LANGUAGE CPP #-}
#ifdef DEFAULT_SIGNATURES
{-# LANGUAGE DefaultSignatures #-}
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
    , emptyArray
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
    -- * Generic toJSON and fromJSON
    , genericToJSON
    , genericFromJSON
    ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.State.Strict
import Control.DeepSeq (NFData(..))
import Data.Aeson.Functions
import Data.Attoparsec.Char8 (Number(..))
import Data.Generics
import Data.Hashable (Hashable(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.IntSet (IntSet)
import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Monoid (Dual(..), First(..), Last(..))
import Data.Monoid (Monoid(..))
import Data.Ratio (Ratio)
import Data.String (IsString(..))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (FormatTime, formatTime, parseTime)
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
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG


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
    rnf (Object o) = rnf o
    rnf (Array a)  = V.foldl' (\x y -> rnf y `seq` x) () a
    rnf (String s) = rnf s
    rnf (Number n) = case n of I i -> rnf i; D d -> rnf d
    rnf (Bool b)   = rnf b
    rnf Null       = ()

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
-- @data Coord { x :: Double, y :: Double }
--
-- instance ToJSON Coord where
--   toJSON (Coord x y) = 'object' [\"x\" '.=' x, \"y\" '.=' y]
-- @
--
-- This example assumes the OverloadedStrings language option is enabled.
class ToJSON a where
    toJSON   :: a -> Value

#ifdef DEFAULT_SIGNATURES
    default toJSON :: Data a => a -> Value
    toJSON = genericToJSON
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
-- @data Coord { x :: Double, y :: Double }
-- 
-- instance FromJSON Coord where
--   parseJSON ('Object' v) = Coord '<$>'
--                         v '.:' \"x\" '<*>'
--                         v '.:' \"y\"
--
--   \-- A non-'Object' value is of the wrong type, so use 'mzero' to fail.
--   parseJSON _          = 'mzero'
-- @
--
-- This example assumes the OverloadedStrings language option is enabled.
class FromJSON a where
    parseJSON :: Value -> Parser a

#ifdef DEFAULT_SIGNATURES
    default parseJSON :: Data a => Value -> Parser a
    parseJSON = genericParseJSON
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


--------------------------------------------------------------------------------
-- Generic toJSON and fromJSON

type T a = a -> Value

genericToJSON :: (Data a) => a -> Value
genericToJSON = toJSON_generic
         `ext1Q` list
         `ext1Q` vector
         `ext1Q` set
         `ext2Q'` mapAny
         `ext2Q'` hashMapAny
         -- Use the standard encoding for all base types.
         `extQ` (toJSON :: T Integer)
         `extQ` (toJSON :: T Int)
         `extQ` (toJSON :: T Int8)
         `extQ` (toJSON :: T Int16)
         `extQ` (toJSON :: T Int32)
         `extQ` (toJSON :: T Int64)
         `extQ` (toJSON :: T Word)
         `extQ` (toJSON :: T Word8)
         `extQ` (toJSON :: T Word16)
         `extQ` (toJSON :: T Word32)
         `extQ` (toJSON :: T Word64)
         `extQ` (toJSON :: T Double)
         `extQ` (toJSON :: T Number)
         `extQ` (toJSON :: T Float)
         `extQ` (toJSON :: T Rational)
         `extQ` (toJSON :: T Char)
         `extQ` (toJSON :: T Text)
         `extQ` (toJSON :: T LT.Text)
         `extQ` (toJSON :: T String)
         `extQ` (toJSON :: T B.ByteString)
         `extQ` (toJSON :: T LB.ByteString)
         `extQ` (toJSON :: T Value)
         `extQ` (toJSON :: T DotNetTime)
         `extQ` (toJSON :: T UTCTime)
         `extQ` (toJSON :: T IntSet)
         `extQ` (toJSON :: T Bool)
         `extQ` (toJSON :: T ())
         --`extQ` (T.toJSON :: T Ordering)
  where
    list xs = Array . V.fromList . map genericToJSON $ xs
    vector v = Array . V.map genericToJSON $ v
    set s = Array . V.fromList . map genericToJSON . Set.toList $ s

    mapAny m
      | tyrep == typeOf T.empty  = remap id
      | tyrep == typeOf LT.empty = remap LT.toStrict
      | tyrep == typeOf string   = remap pack
      | tyrep == typeOf B.empty  = remap decode
      | tyrep == typeOf LB.empty = remap strict
      | otherwise = modError "genericToJSON" $
                             "cannot convert map keyed by type " ++ show tyrep
      where tyrep = typeOf . head . M.keys $ m
            remap f = Object . transformMap (f . fromJust . cast) genericToJSON $ m

    hashMapAny m
      | tyrep == typeOf T.empty  = remap id
      | tyrep == typeOf LT.empty = remap LT.toStrict
      | tyrep == typeOf string   = remap pack
      | tyrep == typeOf B.empty  = remap decode
      | tyrep == typeOf LB.empty = remap strict
      | otherwise = modError "genericToJSON" $
                             "cannot convert map keyed by type " ++ show tyrep
      where tyrep = typeOf . head . H.keys $ m
            remap f = Object . hashMap (f . fromJust . cast) genericToJSON $ m


-- Skip leading '_' in field name so we can use keywords
-- etc. as field names.
mungeField :: String -> Text
mungeField ('_':cs) = pack cs
mungeField cs       = pack cs


toJSON_generic :: (Data a) => a -> Value
toJSON_generic = generic
  where
        -- Generic encoding of an algebraic data type.
        generic a =
            case dataTypeRep (dataTypeOf a) of
                -- No constructor, so it must be an error value.  Code
                -- it anyway as Null.
                AlgRep []  -> Null
                -- Elide a single constructor and just code the arguments.
                AlgRep [c] -> encodeArgs c (gmapQ genericToJSON a)
                -- For multiple constructors, make an object with a
                -- field name that is the constructor (except lower
                -- case) and the data is the arguments encoded.
                AlgRep _   -> encodeConstr (toConstr a) (gmapQ genericToJSON a)
                rep        -> err (dataTypeOf a) rep
           where
              err dt r = modError "genericToJSON" $ "not AlgRep " ++
                                  show r ++ "(" ++ show dt ++ ")"
        -- Encode nullary constructor as a string.
        -- Encode non-nullary constructors as an object with the constructor
        -- name as the single field and the arguments as the value.
        -- Use an array if the are no field names, but elide singleton arrays,
        -- and use an object if there are field names.
        encodeConstr c [] = String . constrString $ c
        encodeConstr c as = object [(constrString c, encodeArgs c as)]

        constrString = pack . showConstr

        encodeArgs c = encodeArgs' (constrFields c)
        encodeArgs' [] [j] = j
        encodeArgs' [] js  = Array . V.fromList $ js
        encodeArgs' ns js  = object $ zip (map mungeField ns) js


genericFromJSON :: (Data a) => Value -> Result a
genericFromJSON = parse genericParseJSON

type F a = Parser a

genericParseJSON :: (Data a) => Value -> Parser a
genericParseJSON j = parseJSON_generic j
             `ext1R` list
             `ext1R` vector
             `ext2R'` mapAny
             `ext2R'` hashMapAny
             -- Use the standard encoding for all base types.
             `extR` (value :: F Integer)
             `extR` (value :: F Int)
             `extR` (value :: F Int8)
             `extR` (value :: F Int16)
             `extR` (value :: F Int32)
             `extR` (value :: F Int64)
             `extR` (value :: F Word)
             `extR` (value :: F Word8)
             `extR` (value :: F Word16)
             `extR` (value :: F Word32)
             `extR` (value :: F Word64)
             `extR` (value :: F Double)
             `extR` (value :: F Number)
             `extR` (value :: F Float)
             `extR` (value :: F Rational)
             `extR` (value :: F Char)
             `extR` (value :: F Text)
             `extR` (value :: F LT.Text)
             `extR` (value :: F String)
             `extR` (value :: F B.ByteString)
             `extR` (value :: F LB.ByteString)
             `extR` (value :: F Value)
             `extR` (value :: F DotNetTime)
             `extR` (value :: F UTCTime)
             `extR` (value :: F IntSet)
             `extR` (value :: F Bool)
             `extR` (value :: F ())
  where
    value :: (FromJSON a) => Parser a
    value = parseJSON j
    list :: (Data a) => Parser [a]
    list = V.toList <$> genericParseJSON j
    vector :: (Data a) => Parser (V.Vector a)
    vector = case j of
               Array js -> V.mapM genericParseJSON js
               _        -> myFail
    mapAny :: forall e f. (Data e, Data f) => Parser (Map f e)
    mapAny
        | tyrep `elem` stringyTypes = res
        | otherwise = myFail
      where res = case j of
                Object js -> M.mapKeysMonotonic trans <$> T.mapM genericParseJSON js
                _         -> myFail
            trans
               | tyrep == typeOf T.empty  = remap id
               | tyrep == typeOf LT.empty = remap LT.fromStrict
               | tyrep == typeOf string   = remap T.unpack
               | tyrep == typeOf B.empty  = remap encodeUtf8
               | tyrep == typeOf LB.empty = remap lazy
               | otherwise = modError "genericParseJSON"
                                      "mapAny -- should never happen"
            tyrep = typeOf (undefined :: f)
            remap f = fromJust . cast . f
    hashMapAny :: forall e f. (Data e, Data f) => Parser (H.HashMap f e)
    hashMapAny
        | tyrep == typeOf string   = process T.unpack
        | tyrep == typeOf LT.empty = process LT.fromStrict
        | tyrep == typeOf T.empty  = process id
        | otherwise = myFail
      where
        process f = maybe myFail return . cast =<< parseWith f
        parseWith :: (Eq c, Hashable c) => (Text -> c) -> Parser (H.HashMap c e)
        parseWith f = case j of
                        Object js -> H.fromList . map (first f) . M.toList <$>
                                     T.mapM genericParseJSON js
                        _          -> myFail
        tyrep = typeOf (undefined :: f)
    myFail = modFail "genericParseJSON" $ "bad data: " ++ show j
    stringyTypes = [typeOf LT.empty, typeOf T.empty, typeOf B.empty,
                    typeOf LB.empty, typeOf string]

parseJSON_generic :: (Data a) => Value -> Parser a
parseJSON_generic j = generic
  where
        typ = dataTypeOf $ resType generic
        generic = case dataTypeRep typ of
                    AlgRep []  -> case j of
                                    Null -> return (modError "genericParseJSON" "empty type")
                                    _ -> modFail "genericParseJSON" "no-constr bad data"
                    AlgRep [_] -> decodeArgs (indexConstr typ 1) j
                    AlgRep _   -> do (c, j') <- getConstr typ j; decodeArgs c j'
                    rep        -> modFail "genericParseJSON" $
                                  show rep ++ "(" ++ show typ ++ ")"
        getConstr t (Object o) | [(s, j')] <- fromJSObject o = do
                                                c <- readConstr' t s
                                                return (c, j')
        getConstr t (String js) = do c <- readConstr' t (unpack js)
                                     return (c, Null) -- handle nullary ctor
        getConstr _ _ = modFail "genericParseJSON" "bad constructor encoding"
        readConstr' t s =
          maybe (modFail "genericParseJSON" $ "unknown constructor: " ++ s ++ " " ++
                         show t)
                return $ readConstr t s

        decodeArgs c0 = go (numConstrArgs (resType generic) c0) c0
                           (constrFields c0)
         where
          go 0 c  _       Null       = construct c []   -- nullary constructor
          go 1 c []       jd         = construct c [jd] -- unary constructor
          go n c []       (Array js)
              | n > 1 = construct c (V.toList js)   -- no field names
          -- FIXME? We could allow reading an array into a constructor
          -- with field names.
          go _ c fs@(_:_) (Object o) = selectFields o fs >>=
                                       construct c -- field names
          go _ c _        jd         = modFail "genericParseJSON" $
                                       "bad decodeArgs data " ++ show (c, jd)

        fromJSObject = map (first unpack) . M.toList

        -- Build the value by stepping through the list of subparts.
        construct c = evalStateT $ fromConstrM f c
          where f :: (Data a) => StateT [Value] Parser a
                f = do js <- get
                       case js of
                         [] -> lift $ modFail "construct" "empty list"
                         (j':js') -> do put js'; lift $ genericParseJSON j'

        -- Select the named fields from a JSON object.
        selectFields fjs = mapM sel
          where sel f = maybe (modFail "genericParseJSON" $ "field does not exist " ++
                               f) return $ M.lookup (mungeField f) fjs

        -- Count how many arguments a constructor has.  The value x is
        -- used to determine what type the constructor returns.
        numConstrArgs :: (Data a) => a -> Constr -> Int
        numConstrArgs x c = execState (fromConstrM f c `asTypeOf` return x) 0
          where f = do modify (+1); return undefined

        resType :: MonadPlus m => m a -> a
        resType _ = modError "genericParseJSON" "resType"

modFail :: (Monad m) => String -> String -> m a
modFail func err = fail $ "Data.Aeson.Types." ++ func ++ ": " ++ err

modError :: String -> String -> a
modError func err = error $ "Data.Aeson.Types." ++ func ++ ": " ++ err

string :: String
string = ""

-- Type extension for binary type constructors.

-- | Flexible type extension
ext2' :: (Data a, Typeable2 t)
     => c a
     -> (forall d1 d2. (Data d1, Data d2) => c (t d1 d2))
     -> c a
ext2' def ext = maybe def id (dataCast2 ext)

-- | Type extension of queries for type constructors
ext2Q' :: (Data d, Typeable2 t)
      => (d -> q)
      -> (forall d1 d2. (Data d1, Data d2) => t d1 d2 -> q)
      -> d -> q
ext2Q' def ext = unQ ((Q def) `ext2'` (Q ext))

-- | Type extension of readers for type constructors
ext2R' :: (Monad m, Data d, Typeable2 t)
      => m d
      -> (forall d1 d2. (Data d1, Data d2) => m (t d1 d2))
      -> m d
ext2R' def ext = unR ((R def) `ext2'` (R ext))

-- | The type constructor for queries
newtype Q q x = Q { unQ :: x -> q }

-- | The type constructor for readers
newtype R m x = R { unR :: m x }
