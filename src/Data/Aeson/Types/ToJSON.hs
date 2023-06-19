{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Types.ToJSON
    (
    -- * Core JSON classes
      ToJSON(..)
    -- * Liftings to unary and binary type constructors
    , ToJSON1(..)
    , toJSON1
    , toEncoding1
    , omitField1
    , ToJSON2(..)
    , toJSON2
    , toEncoding2
    , omitField2
    -- * Generic JSON classes
    , GToJSON'(..)
    , ToArgs(..)
    , genericToJSON
    , genericToEncoding
    , genericLiftToJSON
    , genericLiftToEncoding
    -- * Classes and types for map keys
    , ToJSONKey(..)
    , ToJSONKeyFunction(..)
    , toJSONKeyText
    , toJSONKeyKey
    , contramapToJSONKeyFunction

    , GToJSONKey()
    , genericToJSONKey

    -- * Object key-value pairs
    , KeyValue(..)
    , KeyValueOmit(..)
    , KeyValuePair(..)
    , FromPairs(..)
    -- * Functions needed for documentation
    -- * Encoding functions
    , listEncoding
    , listValue
    ) where

import Data.Aeson.Internal.Prelude

import Control.Monad.ST (ST)
import Data.Aeson.Encoding (Encoding, Encoding', Series, dict, emptyArray_)
import Data.Aeson.Encoding.Internal ((>*<))
import Data.Aeson.Internal.Functions (mapKeyVal, mapKeyValO)
import Data.Aeson.Types.Generic (AllNullary, False, IsRecord, One, ProductSize, Tagged2(..), True, Zero, productSize)
import Data.Aeson.Types.Internal
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Bits (unsafeShiftR)
import Data.DList (DList)
import Data.Fixed (Fixed, HasResolution, Nano)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))
import Data.Functor.These (These1 (..))
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isNothing)
import Data.Ord (Down (..))
import Data.Ratio (Ratio, denominator, numerator)
import Data.Tagged (Tagged(..))
import Data.These (These (..))
import Data.Time (Day, DiffTime, LocalTime, NominalDiffTime, TimeOfDay, ZonedTime)
import Data.Time.Calendar.Month.Compat (Month)
import Data.Time.Calendar.Quarter.Compat (Quarter, QuarterOfYear (..))
import Data.Time.Calendar.Compat (CalendarDiffDays (..), DayOfWeek (..))
import Data.Time.LocalTime.Compat (CalendarDiffTime (..))
import Data.Time.Clock.System.Compat (SystemTime (..))
import Data.Time.Format.Compat (FormatTime, formatTime, defaultTimeLocale)
import Data.Tuple.Solo (Solo (..), getSolo)
import Data.Version (Version, showVersion)
import Foreign.Storable (Storable)
import Foreign.C.Types (CTime (..))
import GHC.Generics
import qualified Data.Aeson.Encoding as E
import qualified Data.Aeson.Encoding.Internal as E (InArray, comma, econcat, retagEncoding, key)
import qualified Data.ByteString.Lazy as L
import qualified Data.DList as DList
import qualified Data.DList.DNonEmpty as DNE
import qualified Data.Fix as F
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Monoid as Monoid
import qualified Data.Scientific as Scientific
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Strict as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Short as ST
import qualified Data.Tree as Tree
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Network.URI as URI

import qualified Data.Aeson.Encoding.Builder as EB
import qualified Data.ByteString.Builder as B

import qualified GHC.Exts as Exts
import qualified Data.Primitive.Array as PM
import qualified Data.Primitive.SmallArray as PM
import qualified Data.Primitive.Types as PM
import qualified Data.Primitive.PrimArray as PM

toJSONPair :: (a -> Value) -> (b -> Value) -> (a, b) -> Value
toJSONPair a b = liftToJSON2 (const False) a (listValue a) (const False) b (listValue b)

realFloatToJSON :: RealFloat a => a -> Value
realFloatToJSON d
    | isNaN d      = Null
    | isInfinite d = if d > 0 then "+inf" else "-inf"
    | otherwise    = Number $ Scientific.fromFloatDigits d

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

-- | Class of generic representation types that can be converted to
-- JSON.
class GToJSON' enc arity f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'toJSON'
    -- (with @enc ~ 'Value'@ and @arity ~ 'Zero'@)
    -- and 'liftToJSON' (if the @arity@ is 'One').
    --
    -- It also provides a generic implementation of 'toEncoding'
    -- (with @enc ~ 'Encoding'@ and @arity ~ 'Zero'@)
    -- and 'liftToEncoding' (if the @arity@ is 'One').
    gToJSON :: Options -> ToArgs enc arity a -> f a -> enc

-- | A 'ToArgs' value either stores nothing (for 'ToJSON') or it stores the three
-- function arguments that encode occurrences of the type parameter (for
-- 'ToJSON1').
data ToArgs res arity a where
    NoToArgs :: ToArgs res Zero a
    To1Args  :: (a -> Bool) -> (a -> res) -> ([a] -> res) -> ToArgs res One a

-- | A configurable generic JSON creator. This function applied to
-- 'defaultOptions' is used as the default for 'toJSON' when the type
-- is an instance of 'Generic'.
genericToJSON :: (Generic a, GToJSON' Value Zero (Rep a))
              => Options -> a -> Value
genericToJSON opts = gToJSON opts NoToArgs . from

-- | A configurable generic JSON creator. This function applied to
-- 'defaultOptions' is used as the default for 'liftToJSON' when the type
-- is an instance of 'Generic1'.
genericLiftToJSON :: (Generic1 f, GToJSON' Value One (Rep1 f))
                  => Options -> (a -> Bool) -> (a -> Value) -> ([a] -> Value)
                  -> f a -> Value
genericLiftToJSON opts o tj tjl = gToJSON opts (To1Args o tj tjl) . from1

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'toEncoding' when the type
-- is an instance of 'Generic'.
genericToEncoding :: (Generic a, GToJSON' Encoding Zero (Rep a))
                  => Options -> a -> Encoding
genericToEncoding opts = gToJSON opts NoToArgs . from

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'liftToEncoding' when the type
-- is an instance of 'Generic1'.
genericLiftToEncoding :: (Generic1 f, GToJSON' Encoding One (Rep1 f))
                      => Options -> (a -> Bool) -> (a -> Encoding) -> ([a] -> Encoding)
                      -> f a -> Encoding
genericLiftToEncoding opts o te tel = gToJSON opts (To1Args o te tel) . from1

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | A type that can be converted to JSON.
--
-- Instances in general /must/ specify 'toJSON' and /should/ (but don't need
-- to) specify 'toEncoding'.
--
-- An example type and instance:
--
-- @
-- \-- Allow ourselves to write 'Text' literals.
-- {-\# LANGUAGE OverloadedStrings #-}
--
-- data Coord = Coord { x :: Double, y :: Double }
--
-- instance 'ToJSON' Coord where
--   'toJSON' (Coord x y) = 'object' [\"x\" '.=' x, \"y\" '.=' y]
--
--   'toEncoding' (Coord x y) = 'pairs' (\"x\" '.=' x '<>' \"y\" '.=' y)
-- @
--
-- Instead of manually writing your 'ToJSON' instance, there are two options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides Template Haskell functions which will derive an
-- instance at compile time. The generated instance is optimized for your type
-- so it will probably be more efficient than the following option.
--
-- * The compiler can provide a default generic implementation for
-- 'toJSON'.
--
-- To use the second, simply add a @deriving 'Generic'@ clause to your
-- datatype and declare a 'ToJSON' instance. If you require nothing other than
-- 'defaultOptions', it is sufficient to write (and this is the only
-- alternative where the default 'toJSON' implementation is sufficient):
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import "GHC.Generics"
--
-- data Coord = Coord { x :: Double, y :: Double } deriving 'Generic'
--
-- instance 'ToJSON' Coord where
--     'toEncoding' = 'genericToEncoding' 'defaultOptions'
-- @
--
-- or more conveniently using the [DerivingVia extension](https://downloads.haskell.org/ghc/9.2.3/docs/html/users_guide/exts/deriving_via.html)
--
-- @
-- deriving via 'Generically' Coord instance 'ToJSON' Coord
-- @
--
-- If on the other hand you wish to customize the generic decoding, you have
-- to implement both methods:
--
-- @
-- customOptions = 'defaultOptions'
--                 { 'fieldLabelModifier' = 'map' 'Data.Char.toUpper'
--                 }
--
-- instance 'ToJSON' Coord where
--     'toJSON'     = 'genericToJSON' customOptions
--     'toEncoding' = 'genericToEncoding' customOptions
-- @
--
-- Previous versions of this library only had the 'toJSON' method. Adding
-- 'toEncoding' had two reasons:
--
-- 1. 'toEncoding' is more efficient for the common case that the output of
-- 'toJSON' is directly serialized to a @ByteString@.
-- Further, expressing either method in terms of the other would be
-- non-optimal.
--
-- 2. The choice of defaults allows a smooth transition for existing users:
-- Existing instances that do not define 'toEncoding' still
-- compile and have the correct semantics. This is ensured by making
-- the default implementation of 'toEncoding' use 'toJSON'. This produces
-- correct results, but since it performs an intermediate conversion to a
-- 'Value', it will be less efficient than directly emitting an 'Encoding'.
-- (this also means that specifying nothing more than
-- @instance ToJSON Coord@ would be sufficient as a generically decoding
-- instance, but there probably exists no good reason to not specify
-- 'toEncoding' in new instances.)
class ToJSON a where
    -- | Convert a Haskell value to a JSON-friendly intermediate type.
    toJSON     :: a -> Value

    default toJSON :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Value
    toJSON = genericToJSON defaultOptions

    -- | Encode a Haskell value as JSON.
    --
    -- The default implementation of this method creates an
    -- intermediate 'Value' using 'toJSON'.  This provides
    -- source-level compatibility for people upgrading from older
    -- versions of this library, but obviously offers no performance
    -- advantage.
    --
    -- To benefit from direct encoding, you /must/ provide an
    -- implementation for this method.  The easiest way to do so is by
    -- having your types implement 'Generic' using the @DeriveGeneric@
    -- extension, and then have GHC generate a method body as follows.
    --
    -- @
    -- instance 'ToJSON' Coord where
    --     'toEncoding' = 'genericToEncoding' 'defaultOptions'
    -- @
    toEncoding :: a -> Encoding
    toEncoding = E.value . toJSON

    toJSONList :: [a] -> Value
    toJSONList = listValue toJSON

    toEncodingList :: [a] -> Encoding
    toEncodingList = listEncoding toEncoding

    -- | Defines when it is acceptable to omit a field of this type from a record.
    -- Used by @('.?=')@ operator, and Generics and TH deriving
    -- with @'omitNothingFields' = True@.
    --
    -- @since 2.2.0.0
    omitField :: a -> Bool
    omitField = const False

-- | @since 2.1.0.0
instance (Generic a, GToJSON' Value Zero (Rep a), GToJSON' Encoding Zero (Rep a)) => ToJSON (Generically a) where
    toJSON     = coerce (genericToJSON     defaultOptions :: a -> Value)
    toEncoding = coerce (genericToEncoding defaultOptions :: a -> Encoding)

-------------------------------------------------------------------------------
-- Object key-value pairs
-------------------------------------------------------------------------------

-- | A key-value pair for encoding a JSON object.
class KeyValue e kv | kv -> e where

    (.=) :: ToJSON v => Key -> v -> kv
    infixr 8 .=

    -- | @since 2.2.0.0
    explicitToField :: (v -> e) -> Key -> v -> kv

instance KeyValue Encoding Series where
    (.=) = explicitToField toEncoding
    {-# INLINE (.=) #-}

    explicitToField f name value = E.pair name (f value)
    {-# INLINE explicitToField #-}

instance (key ~ Key, value ~ Value) => KeyValue Value (key, value) where
    (.=) = explicitToField toJSON
    {-# INLINE (.=) #-}

    explicitToField f name value = (name, f value)
    {-# INLINE explicitToField #-}

-- | Constructs a singleton 'KM.KeyMap'. For calling functions that
--   demand an 'Object' for constructing objects. To be used in
--   conjunction with 'mconcat'. Prefer to use 'object' where possible.
instance value ~ Value => KeyValue Value (KM.KeyMap value) where
    (.=) = explicitToField toJSON
    {-# INLINE (.=) #-}
    
    explicitToField f name value = KM.singleton name (f value)
    {-# INLINE explicitToField #-}

-- | An optional key-value pair for envoding to a JSON object
--
-- @since 2.2.0.0
--
class KeyValue e kv => KeyValueOmit e kv | kv -> e where
    (.?=) :: ToJSON v => Key -> v -> kv
    infixr 8 .?=

    explicitToFieldOmit :: (v -> Bool) -> (v -> e) -> Key -> v -> kv

instance KeyValueOmit Encoding Series where
    name .?= value = if omitField value then mempty else name .= value
    {-# INLINE (.?=) #-}

    explicitToFieldOmit o f name value = if o value then mempty else explicitToField f name value
    {-# INLINE explicitToFieldOmit #-}

instance value ~ Value => KeyValueOmit Value (KM.KeyMap value) where
    name .?= value = if omitField value then KM.empty else name .= value
    {-# INLINE (.?=) #-}

    explicitToFieldOmit o f name value = if o value then KM.empty else explicitToField f name value
    {-# INLINE explicitToFieldOmit #-}

-------------------------------------------------------------------------------
--  Classes and types for map keys
-------------------------------------------------------------------------------

-- | Typeclass for types that can be used as the key of a map-like container
--   (like 'Map' or 'HashMap'). For example, since 'Text' has a 'ToJSONKey'
--   instance and 'Char' has a 'ToJSON' instance, we can encode a value of
--   type 'Map' 'Text' 'Char':
--
--   >>> LBC8.putStrLn $ encode $ Map.fromList [("foo" :: Text, 'a')]
--   {"foo":"a"}
--
--   Since 'Int' also has a 'ToJSONKey' instance, we can similarly write:
--
--   >>> LBC8.putStrLn $ encode $ Map.fromList [(5 :: Int, 'a')]
--   {"5":"a"}
--
--   JSON documents only accept strings as object keys. For any type
--   from @base@ that has a natural textual representation, it can be
--   expected that its 'ToJSONKey' instance will choose that representation.
--
--   For data types that lack a natural textual representation, an alternative
--   is provided. The map-like container is represented as a JSON array
--   instead of a JSON object. Each value in the array is an array with
--   exactly two values. The first is the key and the second is the value.
--
--   For example, values of type '[Text]' cannot be encoded to a
--   string, so a 'Map' with keys of type '[Text]' is encoded as follows:
--
--   >>> LBC8.putStrLn $ encode $ Map.fromList [(["foo","bar","baz" :: Text], 'a')]
--   [[["foo","bar","baz"],"a"]]
--
--   The default implementation of 'ToJSONKey' chooses this method of
--   encoding a key, using the 'ToJSON' instance of the type.
--
--   To use your own data type as the key in a map, all that is needed
--   is to write a 'ToJSONKey' (and possibly a 'FromJSONKey') instance
--   for it. If the type cannot be trivially converted to and from 'Text',
--   it is recommended that 'ToJSONKeyValue' is used. Since the default
--   implementations of the typeclass methods can build this from a
--   'ToJSON' instance, there is nothing that needs to be written:
--
--   > data Foo = Foo { fooAge :: Int, fooName :: Text }
--   >   deriving (Eq,Ord,Generic)
--   > instance ToJSON Foo
--   > instance ToJSONKey Foo
--
--   That's it. We can now write:
--
--   >>> let m = Map.fromList [(Foo 4 "bar",'a'),(Foo 6 "arg",'b')]
--   >>> LBC8.putStrLn $ encode m
--   [[{"fooName":"bar","fooAge":4},"a"],[{"fooName":"arg","fooAge":6},"b"]]
--
--   The next case to consider is if we have a type that is a
--   newtype wrapper around 'Text'. The recommended approach is to use
--   generalized newtype deriving:
--
--   > newtype RecordId = RecordId { getRecordId :: Text }
--   >   deriving (Eq,Ord,ToJSONKey)
--
--   Then we may write:
--
--   >>> LBC8.putStrLn $ encode $ Map.fromList [(RecordId "abc",'a')]
--   {"abc":"a"}
--
--   Simple sum types are a final case worth considering. Suppose we have:
--
--   > data Color = Red | Green | Blue
--   >   deriving (Show,Read,Eq,Ord)
--
--   It is possible to get the 'ToJSONKey' instance for free as we did
--   with 'Foo'. However, in this case, we have a natural way to go to
--   and from 'Text' that does not require any escape sequences. So
--   'ToJSONKeyText' can be used instead of 'ToJSONKeyValue' to encode maps
--   as objects instead of arrays of pairs. This instance may be
--   implemented using generics as follows:
--
-- @
-- instance 'ToJSONKey' Color where
--   'toJSONKey' = 'genericToJSONKey' 'defaultJSONKeyOptions'
-- @
--
--   === __Low-level implementations__
--
--   The 'Show' instance can be used to help write 'ToJSONKey':
--
--   > instance ToJSONKey Color where
--   >   toJSONKey = ToJSONKeyText f g
--   >     where f = Text.pack . show
--   >           g = text . Text.pack . show
--   >           -- text function is from Data.Aeson.Encoding
--
--   The situation of needing to turning function @a -> Text@ into
--   a 'ToJSONKeyFunction' is common enough that a special combinator
--   is provided for it. The above instance can be rewritten as:
--
--   > instance ToJSONKey Color where
--   >   toJSONKey = toJSONKeyText (Text.pack . show)
--
--   The performance of the above instance can be improved by
--   not using 'String' as an intermediate step when converting to
--   'Text'. One option for improving performance would be to use
--   template haskell machinery from the @text-show@ package. However,
--   even with the approach, the 'Encoding' (a wrapper around a bytestring
--   builder) is generated by encoding the 'Text' to a 'ByteString',
--   an intermediate step that could be avoided. The fastest possible
--   implementation would be:
--
--   > -- Assuming that OverloadedStrings is enabled
--   > instance ToJSONKey Color where
--   >   toJSONKey = ToJSONKeyText f g
--   >     where f x = case x of {Red -> "Red";Green ->"Green";Blue -> "Blue"}
--   >           g x = case x of {Red -> text "Red";Green -> text "Green";Blue -> text "Blue"}
--   >           -- text function is from Data.Aeson.Encoding
--
--   This works because GHC can lift the encoded values out of the case
--   statements, which means that they are only evaluated once. This
--   approach should only be used when there is a serious need to
--   maximize performance.

class ToJSONKey a where
    -- | Strategy for rendering the key for a map-like container.
    toJSONKey :: ToJSONKeyFunction a
    default toJSONKey :: ToJSON a => ToJSONKeyFunction a
    toJSONKey = ToJSONKeyValue toJSON toEncoding

    -- | This is similar in spirit to the 'showsList' method of 'Show'.
    --   It makes it possible to give 'String' keys special treatment
    --   without using @OverlappingInstances@. End users should always
    --   be able to use the default implementation of this method.
    toJSONKeyList :: ToJSONKeyFunction [a]
    default toJSONKeyList :: ToJSON a => ToJSONKeyFunction [a]
    toJSONKeyList = ToJSONKeyValue toJSON toEncoding

data ToJSONKeyFunction a
    = ToJSONKeyText !(a -> Key) !(a -> Encoding' Key)
      -- ^ key is encoded to string, produces object
    | ToJSONKeyValue !(a -> Value) !(a -> Encoding)
      -- ^ key is encoded to value, produces array

-- | Helper for creating textual keys.
--
-- @
-- instance 'ToJSONKey' MyKey where
--     'toJSONKey' = 'toJSONKeyText' myKeyToText
--       where
--         myKeyToText = Text.pack . show -- or showt from text-show
-- @
toJSONKeyText :: (a -> Text) -> ToJSONKeyFunction a
toJSONKeyText f = toJSONKeyKey (Key.fromText . f)

-- |
--
-- @since 2.0.0.0
toJSONKeyKey :: (a -> Key) -> ToJSONKeyFunction a
toJSONKeyKey f = ToJSONKeyText f (E.key . f)

-- | TODO: should this be exported?
toJSONKeyTextEnc :: (a -> Encoding' Key) -> ToJSONKeyFunction a
toJSONKeyTextEnc e = ToJSONKeyText tot e
 where
    -- TODO: dropAround is also used in stringEncoding, which is unfortunate atm
    tot = Key.fromText
        . T.dropAround (== '"')
        . T.decodeLatin1
        . L.toStrict
        . E.encodingToLazyByteString
        . e

instance Contravariant ToJSONKeyFunction where
    contramap = contramapToJSONKeyFunction

-- | Contravariant map, as 'ToJSONKeyFunction' is a contravariant functor.
contramapToJSONKeyFunction :: (b -> a) -> ToJSONKeyFunction a -> ToJSONKeyFunction b
contramapToJSONKeyFunction h x = case x of
    ToJSONKeyText  f g -> ToJSONKeyText (f . h) (g . h)
    ToJSONKeyValue f g -> ToJSONKeyValue (f . h) (g . h)

-- 'toJSONKey' for 'Generic' types.
-- Deriving is supported for enumeration types, i.e. the sums of nullary
-- constructors. The names of constructors will be used as keys for JSON
-- objects.
--
-- See also 'genericFromJSONKey'.
--
-- === __Example__
--
-- @
-- data Color = Red | Green | Blue
--   deriving 'Generic'
--
-- instance 'ToJSONKey' Color where
--   'toJSONKey' = 'genericToJSONKey' 'defaultJSONKeyOptions'
-- @
genericToJSONKey :: (Generic a, GToJSONKey (Rep a))
           => JSONKeyOptions -> ToJSONKeyFunction a
genericToJSONKey opts = toJSONKeyKey (Key.fromString . keyModifier opts . getConName . from)

class    GetConName f => GToJSONKey f
instance GetConName f => GToJSONKey f

-------------------------------------------------------------------------------
-- Liftings of FromJSON and ToJSON to unary and binary type constructors
-------------------------------------------------------------------------------


-- | Lifting of the 'ToJSON' class to unary type constructors.
--
-- Instead of manually writing your 'ToJSON1' instance, there are two options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides Template Haskell functions which will derive an
-- instance at compile time. The generated instance is optimized for your type
-- so it will probably be more efficient than the following option.
--
-- * The compiler can provide a default generic implementation for
-- 'toJSON1'.
--
-- To use the second, simply add a @deriving 'Generic1'@ clause to your
-- datatype and declare a 'ToJSON1' instance for your datatype without giving
-- definitions for 'liftToJSON' or 'liftToEncoding'.
--
-- For example:
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import "GHC.Generics"
--
-- data Pair a b = Pair { pairFst :: a, pairSnd :: b } deriving 'Generic1'
--
-- instance 'ToJSON' a => 'ToJSON1' (Pair a)
-- @
--
-- If the default implementation doesn't give exactly the results you want,
-- you can customize the generic encoding with only a tiny amount of
-- effort, using 'genericLiftToJSON' and 'genericLiftToEncoding' with
-- your preferred 'Options':
--
-- @
-- customOptions = 'defaultOptions'
--                 { 'fieldLabelModifier' = 'map' 'Data.Char.toUpper'
--                 }
--
-- instance 'ToJSON' a => 'ToJSON1' (Pair a) where
--     'liftToJSON'     = 'genericLiftToJSON' customOptions
--     'liftToEncoding' = 'genericLiftToEncoding' customOptions
-- @
--
-- See also 'ToJSON'.
class ToJSON1 f where
    liftToJSON :: (a -> Bool) -> (a -> Value) -> ([a] -> Value) -> f a -> Value

    default liftToJSON :: (Generic1 f, GToJSON' Value One (Rep1 f))
                       => (a -> Bool) -> (a -> Value) -> ([a] -> Value) -> f a -> Value
    liftToJSON = genericLiftToJSON defaultOptions

    liftToJSONList :: (a -> Bool) -> (a -> Value) -> ([a] -> Value) -> [f a] -> Value
    liftToJSONList o f g = listValue (liftToJSON o f g)

    liftToEncoding :: (a -> Bool) -> (a -> Encoding) -> ([a] -> Encoding) -> f a -> Encoding

    default liftToEncoding :: (Generic1 f, GToJSON' Encoding One (Rep1 f))
                           => (a -> Bool) -> (a -> Encoding) -> ([a] -> Encoding)
                           -> f a -> Encoding
    liftToEncoding = genericLiftToEncoding defaultOptions

    liftToEncodingList :: (a -> Bool) -> (a -> Encoding) -> ([a] -> Encoding) -> [f a] -> Encoding
    liftToEncodingList o f g = listEncoding (liftToEncoding o f g)

    -- | @since 2.2.0.0
    liftOmitField :: (a -> Bool) -> f a -> Bool
    liftOmitField _ _ = False

-- | @since 2.1.0.0
instance (Generic1 f, GToJSON' Value One (Rep1 f), GToJSON' Encoding One (Rep1 f)) => ToJSON1 (Generically1 f) where
    liftToJSON :: forall a. (a -> Bool) -> (a -> Value) -> ([a] -> Value) -> Generically1 f a -> Value
    liftToJSON = coerce (genericLiftToJSON defaultOptions :: (a -> Bool) -> (a -> Value) -> ([a] -> Value) -> f a -> Value)

    liftToEncoding :: forall a. (a -> Bool) -> (a -> Encoding) -> ([a] -> Encoding) -> Generically1 f a -> Encoding
    liftToEncoding = coerce (genericLiftToEncoding defaultOptions :: (a -> Bool) -> (a -> Encoding) -> ([a] -> Encoding) -> f a -> Encoding)

-- | Lift the standard 'toJSON' function through the type constructor.
toJSON1 :: (ToJSON1 f, ToJSON a) => f a -> Value
toJSON1 = liftToJSON omitField toJSON toJSONList
{-# INLINE toJSON1 #-}

-- | Lift the standard 'toEncoding' function through the type constructor.
toEncoding1 :: (ToJSON1 f, ToJSON a) => f a -> Encoding
toEncoding1 = liftToEncoding omitField toEncoding toEncodingList
{-# INLINE toEncoding1 #-}

omitField1 :: (ToJSON1 f, ToJSON a) => f a -> Bool
omitField1 = liftOmitField omitField

-- | Lifting of the 'ToJSON' class to binary type constructors.
--
-- Instead of manually writing your 'ToJSON2' instance, "Data.Aeson.TH"
-- provides Template Haskell functions which will derive an instance at compile time.
--
-- The compiler cannot provide a default generic implementation for 'liftToJSON2',
-- unlike 'toJSON' and 'liftToJSON'.
class ToJSON2 f where
    liftToJSON2 :: (a -> Bool) -> (a -> Value) -> ([a] -> Value) -> (b -> Bool) -> (b -> Value) -> ([b] -> Value) -> f a b -> Value
    liftToJSONList2 ::  (a -> Bool) -> (a -> Value) -> ([a] -> Value) -> (b -> Bool) -> (b -> Value) -> ([b] -> Value) -> [f a b] -> Value
    liftToJSONList2 oa fa ga ob fb gb = listValue (liftToJSON2 oa fa ga ob fb gb)

    liftToEncoding2 ::  (a -> Bool) -> (a -> Encoding) -> ([a] -> Encoding) -> (b -> Bool) -> (b -> Encoding) -> ([b] -> Encoding) -> f a b -> Encoding
    liftToEncodingList2 ::  (a -> Bool) -> (a -> Encoding) -> ([a] -> Encoding) -> (b -> Bool) -> (b -> Encoding) -> ([b] -> Encoding) -> [f a b] -> Encoding
    liftToEncodingList2 oa fa ga ob fb gb = listEncoding (liftToEncoding2 oa fa ga ob fb gb)

    -- | @since 2.2.0.0
    liftOmitField2 :: (a -> Bool) -> (b -> Bool) -> f a b -> Bool
    liftOmitField2 _ _ _ = False

-- | Lift the standard 'toJSON' function through the type constructor.
toJSON2 :: (ToJSON2 f, ToJSON a, ToJSON b) => f a b -> Value
toJSON2 = liftToJSON2 omitField toJSON toJSONList omitField toJSON toJSONList
{-# INLINE toJSON2 #-}

-- | Lift the standard 'toEncoding' function through the type constructor.
toEncoding2 :: (ToJSON2 f, ToJSON a, ToJSON b) => f a b -> Encoding
toEncoding2 = liftToEncoding2 omitField toEncoding toEncodingList omitField toEncoding toEncodingList
{-# INLINE toEncoding2 #-}

omitField2 :: (ToJSON2 f, ToJSON a, ToJSON b) => f a b -> Bool
omitField2 = liftOmitField2 omitField omitField
{-# INLINE omitField2 #-}

-------------------------------------------------------------------------------
-- Encoding functions
-------------------------------------------------------------------------------

-- | Helper function to use with 'liftToEncoding'.
-- Useful when writing own 'ToJSON1' instances.
--
-- @
-- newtype F a = F [a]
--
-- -- This instance encodes 'String' as an array of chars
-- instance 'ToJSON1' F where
--     'liftToJSON'     tj _ (F xs) = 'liftToJSON'     tj ('listValue'    tj) xs
--     'liftToEncoding' te _ (F xs) = 'liftToEncoding' te ('listEncoding' te) xs
--
-- instance 'Data.Aeson.FromJSON.FromJSON1' F where
--     'Data.Aeson.FromJSON.liftParseJSON' p _ v = F \<$\> 'Data.Aeson.FromJSON.liftParseJSON' p ('Data.Aeson.FromJSON.listParser' p) v
-- @
listEncoding :: (a -> Encoding) -> [a] -> Encoding
listEncoding = E.list
{-# INLINE listEncoding #-}

-- | Helper function to use with 'liftToJSON', see 'listEncoding'.
listValue :: (a -> Value) -> [a] -> Value
listValue f = Array . V.fromList . map f
{-# INLINE listValue #-}

-------------------------------------------------------------------------------
-- [] instances
-------------------------------------------------------------------------------

-- These are needed for key-class default definitions

instance ToJSON1 [] where
    liftToJSON _ _ to' = to'

    liftToEncoding _ _ to' = to'

instance (ToJSON a) => ToJSON [a] where
    {-# SPECIALIZE instance ToJSON String #-}
    {-# SPECIALIZE instance ToJSON [String] #-}
    {-# SPECIALIZE instance ToJSON [Array] #-}
    {-# SPECIALIZE instance ToJSON [Object] #-}

    toJSON = toJSON1

    toEncoding = toEncoding1

-------------------------------------------------------------------------------
-- Generic toJSON / toEncoding
-------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (GToJSON' enc arity a) => GToJSON' enc arity (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is ignored:
    gToJSON opts targs = gToJSON opts targs . unM1
    {-# INLINE gToJSON #-}

instance GToJSON' enc One Par1 where
    -- Direct occurrences of the last type parameter are encoded with the
    -- function passed in as an argument:
    gToJSON _opts (To1Args _ tj _) = tj . unPar1 -- TODO
    {-# INLINE gToJSON #-}

instance ( ConsToJSON enc arity a
         , AllNullary          (C1 c a) allNullary
         , SumToJSON enc arity (C1 c a) allNullary
         ) => GToJSON' enc arity (D1 d (C1 c a)) where
    -- The option 'tagSingleConstructors' determines whether to wrap
    -- a single-constructor type.
    gToJSON opts targs
        | tagSingleConstructors opts = (unTagged :: Tagged allNullary enc -> enc)
                                     . sumToJSON opts targs
                                     . unM1
        | otherwise = consToJSON opts targs . unM1 . unM1
    {-# INLINE gToJSON #-}

instance (ConsToJSON enc arity a) => GToJSON' enc arity (C1 c a) where
    -- Constructors need to be encoded differently depending on whether they're
    -- a record or not. This distinction is made by 'consToJSON':
    gToJSON opts targs = consToJSON opts targs . unM1
    {-# INLINE gToJSON #-}

instance ( AllNullary       (a :+: b) allNullary
         , SumToJSON  enc arity (a :+: b) allNullary
         ) => GToJSON' enc arity (a :+: b)
  where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are encoded to
    -- strings.  This distinction is made by 'sumToJSON':
    gToJSON opts targs = (unTagged :: Tagged allNullary enc -> enc)
                       . sumToJSON opts targs
    {-# INLINE gToJSON #-}

--------------------------------------------------------------------------------
-- Generic toJSON

-- Note: Refactoring 'ToJSON a' to 'ToJSON enc a' (and 'ToJSON1' similarly) is
-- possible but makes error messages a bit harder to understand for missing
-- instances.

instance GToJSON' Value arity V1 where
    -- Empty values do not exist, which makes the job of formatting them
    -- rather easy:
    gToJSON _ _ x = x `seq` error "case: V1"
    {-# INLINE gToJSON #-}

instance ToJSON a => GToJSON' Value arity (K1 i a) where
    -- Constant values are encoded using their ToJSON instance:
    gToJSON _opts _ = toJSON . unK1
    {-# INLINE gToJSON #-}

instance ToJSON1 f => GToJSON' Value One (Rec1 f) where
    -- Recursive occurrences of the last type parameter are encoded using their
    -- ToJSON1 instance:
    gToJSON _opts (To1Args o tj tjl) = liftToJSON o tj tjl . unRec1
    {-# INLINE gToJSON #-}

instance GToJSON' Value arity U1 where
    -- Empty constructors are encoded to an empty array:
    gToJSON _opts _ _ = emptyArray
    {-# INLINE gToJSON #-}

instance ( WriteProduct arity a, WriteProduct arity b
         , ProductSize        a, ProductSize        b
         ) => GToJSON' Value arity (a :*: b)
  where
    -- Products are encoded to an array. Here we allocate a mutable vector of
    -- the same size as the product and write the product's elements to it using
    -- 'writeProduct':
    gToJSON opts targs p =
        Array $ V.create $ do
          mv <- VM.unsafeNew lenProduct
          writeProduct opts targs mv 0 lenProduct p
          return mv
        where
          lenProduct = (unTagged2 :: Tagged2 (a :*: b) Int -> Int)
                       productSize
    {-# INLINE gToJSON #-}

instance ( ToJSON1 f
         , GToJSON' Value One g
         ) => GToJSON' Value One (f :.: g)
  where
    -- If an occurrence of the last type parameter is nested inside two
    -- composed types, it is encoded by using the outermost type's ToJSON1
    -- instance to generically encode the innermost type:
    gToJSON opts targs =
      let gtj = gToJSON opts targs in
      liftToJSON (const False) gtj (listValue gtj) . unComp1
    {-# INLINE gToJSON #-}

--------------------------------------------------------------------------------
-- Generic toEncoding

instance GToJSON' Encoding arity V1 where
    -- Empty values do not exist, which makes the job of formatting them
    -- rather easy:
    gToJSON _ _ x = case x of {}
    {-# INLINE gToJSON #-}

instance ToJSON a => GToJSON' Encoding arity (K1 i a) where
    -- Constant values are encoded using their ToJSON instance:
    gToJSON _opts _ = toEncoding . unK1
    {-# INLINE gToJSON #-}

instance ToJSON1 f => GToJSON' Encoding One (Rec1 f) where
    -- Recursive occurrences of the last type parameter are encoded using their
    -- ToEncoding1 instance:
    gToJSON _opts (To1Args o te tel) = liftToEncoding o te tel . unRec1
    {-# INLINE gToJSON #-}

instance GToJSON' Encoding arity U1 where
    -- Empty constructors are encoded to an empty array:
    gToJSON _opts _ _ = E.emptyArray_
    {-# INLINE gToJSON #-}

instance ( EncodeProduct  arity a
         , EncodeProduct  arity b
         ) => GToJSON' Encoding arity (a :*: b)
  where
    -- Products are encoded to an array. Here we allocate a mutable vector of
    -- the same size as the product and write the product's elements to it using
    -- 'encodeProduct':
    gToJSON opts targs p = E.list E.retagEncoding [encodeProduct opts targs p]
    {-# INLINE gToJSON #-}

instance ( ToJSON1 f
         , GToJSON' Encoding One g
         ) => GToJSON' Encoding One (f :.: g)
  where
    -- If an occurrence of the last type parameter is nested inside two
    -- composed types, it is encoded by using the outermost type's ToJSON1
    -- instance to generically encode the innermost type:
    gToJSON opts targs =
      let gte = gToJSON opts targs in
      liftToEncoding (const False) gte (listEncoding gte) . unComp1
    {-# INLINE gToJSON #-}

--------------------------------------------------------------------------------

class SumToJSON enc arity f allNullary where
    sumToJSON :: Options -> ToArgs enc arity a
              -> f a -> Tagged allNullary enc

instance ( GetConName f
         , IsString enc
         , TaggedObject                     enc arity f
         , SumToJSON' ObjectWithSingleField enc arity f
         , SumToJSON' TwoElemArray          enc arity f
         , SumToJSON' UntaggedValue         enc arity f
         ) => SumToJSON enc arity f True
  where
    sumToJSON opts targs
        | allNullaryToStringTag opts = Tagged . fromString
                                     . constructorTagModifier opts . getConName
        | otherwise = Tagged . nonAllNullarySumToJSON opts targs
    {-# INLINE sumToJSON #-}

instance ( TaggedObject                     enc arity f
         , SumToJSON' ObjectWithSingleField enc arity f
         , SumToJSON' TwoElemArray          enc arity f
         , SumToJSON' UntaggedValue         enc arity f
         ) => SumToJSON enc arity f False
  where
    sumToJSON opts targs = Tagged . nonAllNullarySumToJSON opts targs
    {-# INLINE sumToJSON #-}

nonAllNullarySumToJSON :: ( TaggedObject                     enc arity f
                          , SumToJSON' ObjectWithSingleField enc arity f
                          , SumToJSON' TwoElemArray          enc arity f
                          , SumToJSON' UntaggedValue         enc arity f
                          ) => Options -> ToArgs enc arity a
                            -> f a -> enc
nonAllNullarySumToJSON opts targs =
    case sumEncoding opts of

      TaggedObject{..}      ->
        taggedObject opts targs (Key.fromString tagFieldName) (Key.fromString contentsFieldName)

      ObjectWithSingleField ->
        (unTagged :: Tagged ObjectWithSingleField enc -> enc)
          . sumToJSON' opts targs

      TwoElemArray          ->
        (unTagged :: Tagged TwoElemArray enc -> enc)
          . sumToJSON' opts targs

      UntaggedValue         ->
        (unTagged :: Tagged UntaggedValue enc -> enc)
          . sumToJSON' opts targs
{-# INLINE nonAllNullarySumToJSON #-}

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

class TaggedObject enc arity f where
    taggedObject :: Options -> ToArgs enc arity a
                 -> Key -> Key
                 -> f a -> enc

instance ( TaggedObject enc arity a
         , TaggedObject enc arity b
         ) => TaggedObject enc arity (a :+: b)
  where
    taggedObject opts targs tagFieldName contentsFieldName (L1 x) =
        taggedObject opts targs tagFieldName contentsFieldName x
    taggedObject opts targs tagFieldName contentsFieldName (R1 x) =
        taggedObject opts targs tagFieldName contentsFieldName x
    {-# INLINE taggedObject #-}

instance ( IsRecord                      a isRecord
         , TaggedObject' enc pairs arity a isRecord
         , FromPairs enc pairs
         , IsString enc
         , KeyValuePair enc pairs
         , Constructor c
         ) => TaggedObject enc arity (C1 c a)
  where
    taggedObject opts targs tagFieldName contentsFieldName =
      fromPairs . mappend tag . contents
      where
        tag = tagFieldName `pair`
          (fromString (constructorTagModifier opts (conName (undefined :: t c a p)))
            :: enc)
        contents =
          (unTagged :: Tagged isRecord pairs -> pairs) .
            taggedObject' opts targs contentsFieldName . unM1
    {-# INLINE taggedObject #-}

class TaggedObject' enc pairs arity f isRecord where
    taggedObject' :: Options -> ToArgs enc arity a
                  -> Key -> f a -> Tagged isRecord pairs

instance ( GToJSON' enc arity f
         , KeyValuePair enc pairs
         ) => TaggedObject' enc pairs arity f False
  where
    taggedObject' opts targs contentsFieldName =
        Tagged . (contentsFieldName `pair`) . gToJSON opts targs
    {-# INLINE taggedObject' #-}

instance {-# OVERLAPPING #-} Monoid pairs => TaggedObject' enc pairs arity U1 False where
    taggedObject' _ _ _ _ = Tagged mempty
    {-# INLINE taggedObject' #-}

instance ( RecordToPairs enc pairs arity f
         ) => TaggedObject' enc pairs arity f True
  where
    taggedObject' opts targs _ = Tagged . recordToPairs opts targs
    {-# INLINE taggedObject' #-}

--------------------------------------------------------------------------------

-- | Get the name of the constructor of a sum datatype.
class GetConName f where
    getConName :: f a -> String

instance (GetConName a, GetConName b) => GetConName (a :+: b) where
    getConName (L1 x) = getConName x
    getConName (R1 x) = getConName x
    {-# INLINE getConName #-}

instance (Constructor c) => GetConName (C1 c a) where
    getConName = conName
    {-# INLINE getConName #-}

-- For genericToJSONKey
instance GetConName a => GetConName (D1 d a) where
    getConName (M1 x) = getConName x
    {-# INLINE getConName #-}

--------------------------------------------------------------------------------

-- Reflection of SumEncoding variants

data ObjectWithSingleField
data TwoElemArray
data UntaggedValue

--------------------------------------------------------------------------------

class SumToJSON' s enc arity f where
    sumToJSON' :: Options -> ToArgs enc arity a
                    -> f a -> Tagged s enc

instance ( SumToJSON' s enc arity a
         , SumToJSON' s enc arity b
         ) => SumToJSON' s enc arity (a :+: b)
  where
    sumToJSON' opts targs (L1 x) = sumToJSON' opts targs x
    sumToJSON' opts targs (R1 x) = sumToJSON' opts targs x
    {-# INLINE sumToJSON' #-}

--------------------------------------------------------------------------------

instance ( GToJSON'    Value arity a
         , ConsToJSON Value arity a
         , Constructor c
         ) => SumToJSON' TwoElemArray Value arity (C1 c a) where
    sumToJSON' opts targs x = Tagged $ Array $ V.create $ do
      mv <- VM.unsafeNew 2
      VM.unsafeWrite mv 0 $ String $ T.pack $ constructorTagModifier opts
                                   $ conName (undefined :: t c a p)
      VM.unsafeWrite mv 1 $ gToJSON opts targs x
      return mv
    {-# INLINE sumToJSON' #-}

--------------------------------------------------------------------------------

instance ( GToJSON'    Encoding arity a
         , ConsToJSON Encoding arity a
         , Constructor c
         ) => SumToJSON' TwoElemArray Encoding arity (C1 c a)
  where
    sumToJSON' opts targs x = Tagged $ E.list id
      [ toEncoding (constructorTagModifier opts (conName (undefined :: t c a p)))
      , gToJSON opts targs x
      ]
    {-# INLINE sumToJSON' #-}

--------------------------------------------------------------------------------

class ConsToJSON enc arity f where
    consToJSON :: Options -> ToArgs enc arity a
               -> f a -> enc

class ConsToJSON' enc arity f isRecord where
    consToJSON'     :: Options -> ToArgs enc arity a
                    -> f a -> Tagged isRecord enc

instance ( IsRecord                f isRecord
         , ConsToJSON'   enc arity f isRecord
         ) => ConsToJSON enc arity f
  where
    consToJSON opts targs =
        (unTagged :: Tagged isRecord enc -> enc)
      . consToJSON' opts targs
    {-# INLINE consToJSON #-}

instance {-# OVERLAPPING #-}
         ( RecordToPairs enc pairs arity (S1 s f)
         , FromPairs enc pairs
         , GToJSON' enc arity f
         ) => ConsToJSON' enc arity (S1 s f) True
  where
    consToJSON' opts targs
      | unwrapUnaryRecords opts = Tagged . gToJSON opts targs
      | otherwise = Tagged . fromPairs . recordToPairs opts targs
    {-# INLINE consToJSON' #-}

instance ( RecordToPairs enc pairs arity f
         , FromPairs enc pairs
         ) => ConsToJSON' enc arity f True
  where
    consToJSON' opts targs = Tagged . fromPairs . recordToPairs opts targs
    {-# INLINE consToJSON' #-}

instance GToJSON' enc arity f => ConsToJSON' enc arity f False where
    consToJSON' opts targs = Tagged . gToJSON opts targs
    {-# INLINE consToJSON' #-}

--------------------------------------------------------------------------------

class RecordToPairs enc pairs arity f where
    -- 1st element: whole thing
    -- 2nd element: in case the record has only 1 field, just the value
    --              of the field (without the key); 'Nothing' otherwise
    recordToPairs :: Options -> ToArgs enc arity a
                  -> f a -> pairs

instance ( Monoid pairs
         , RecordToPairs enc pairs arity a
         , RecordToPairs enc pairs arity b
         ) => RecordToPairs enc pairs arity (a :*: b)
  where
    recordToPairs opts (targs :: ToArgs enc arity p) (a :*: b) =
        pairsOf a `mappend` pairsOf b
      where
        pairsOf :: (RecordToPairs enc pairs arity f) => f p -> pairs
        pairsOf = recordToPairs opts targs
    {-# INLINE recordToPairs #-}

instance ( Selector s
         , GToJSON' enc arity (K1 i t)
         , KeyValuePair enc pairs
         , ToJSON t
         ) => RecordToPairs enc pairs arity (S1 s (K1 i t))
  where
    recordToPairs opts targs m1
      | omitNothingFields opts
      , omitField (unK1 $ unM1 m1 :: t)
      = mempty

      | otherwise =
        let key   = Key.fromString $ fieldLabelModifier opts (selName m1)
            value = gToJSON opts targs (unM1 m1)
         in key `pair` value
    {-# INLINE recordToPairs #-}

instance ( Selector s
         , GToJSON' enc One (Rec1 f)
         , KeyValuePair enc pairs
         , ToJSON1 f
         ) => RecordToPairs enc pairs One (S1 s (Rec1 f))
  where
    recordToPairs opts targs@(To1Args o _ _) m1
      | omitNothingFields opts
      , liftOmitField o $ unRec1 $ unM1 m1
      = mempty

      | otherwise =
        let key   = Key.fromString $ fieldLabelModifier opts (selName m1)
            value = gToJSON opts targs (unM1 m1)
            in key `pair` value
    {-# INLINE recordToPairs #-}

instance ( Selector s
         , GToJSON' enc One Par1
         , KeyValuePair enc pairs
         ) => RecordToPairs enc pairs One (S1 s Par1)
  where
    recordToPairs opts targs@(To1Args o _ _) m1
      | omitNothingFields opts
      , o (unPar1 (unM1 m1))
      = mempty

      | otherwise =
        let key   = Key.fromString $ fieldLabelModifier opts (selName m1)
            value = gToJSON opts targs (unM1 m1)
          in key `pair` value
    {-# INLINE recordToPairs #-}

--------------------------------------------------------------------------------

class WriteProduct arity f where
    writeProduct :: Options
                 -> ToArgs Value arity a
                 -> VM.MVector s Value
                 -> Int -- ^ index
                 -> Int -- ^ length
                 -> f a
                 -> ST s ()

instance ( WriteProduct arity a
         , WriteProduct arity b
         ) => WriteProduct arity (a :*: b) where
    writeProduct opts targs mv ix len (a :*: b) = do
      writeProduct opts targs mv ix  lenL a
      writeProduct opts targs mv ixR lenR b
        where
          lenL = len `unsafeShiftR` 1
          lenR = len - lenL
          ixR  = ix  + lenL
    {-# INLINE writeProduct #-}

instance {-# OVERLAPPABLE #-} (GToJSON' Value arity a) => WriteProduct arity a where
    writeProduct opts targs mv ix _ =
      VM.unsafeWrite mv ix . gToJSON opts targs
    {-# INLINE writeProduct #-}

--------------------------------------------------------------------------------

class EncodeProduct arity f where
    encodeProduct :: Options -> ToArgs Encoding arity a
                  -> f a -> Encoding' E.InArray

instance ( EncodeProduct    arity a
         , EncodeProduct    arity b
         ) => EncodeProduct arity (a :*: b) where
    encodeProduct opts targs (a :*: b) | omitNothingFields opts =
        E.econcat $ intersperse E.comma $
        filter (not . E.nullEncoding)
        [encodeProduct opts targs a, encodeProduct opts targs b]
    encodeProduct opts targs (a :*: b) =
      encodeProduct opts targs a >*<
      encodeProduct opts targs b
    {-# INLINE encodeProduct #-}

instance {-# OVERLAPPABLE #-} (GToJSON' Encoding arity a) => EncodeProduct arity a where
    encodeProduct opts targs a = E.retagEncoding $ gToJSON opts targs a
    {-# INLINE encodeProduct #-}

--------------------------------------------------------------------------------

instance ( GToJSON'   enc arity a
         , ConsToJSON enc arity a
         , FromPairs  enc pairs
         , KeyValuePair  enc pairs
         , Constructor c
         ) => SumToJSON' ObjectWithSingleField enc arity (C1 c a)
  where
    sumToJSON' opts targs =
      Tagged . fromPairs . (typ `pair`) . gToJSON opts targs
        where
          typ = Key.fromString $ constructorTagModifier opts $
                         conName (undefined :: t c a p)
    {-# INLINE sumToJSON' #-}

--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-}
    ( ConsToJSON enc arity a
    ) => SumToJSON' UntaggedValue enc arity (C1 c a)
  where
    sumToJSON' opts targs = Tagged . gToJSON opts targs
    {-# INLINE sumToJSON' #-}

instance {-# OVERLAPPING #-}
    ( Constructor c
    , IsString enc
    ) => SumToJSON' UntaggedValue enc arity (C1 c U1)
  where
    sumToJSON' opts _ _ = Tagged . fromString $
        constructorTagModifier opts $ conName (undefined :: t c U1 p)
    {-# INLINE sumToJSON' #-}

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance ToJSON2 Const where
    liftToJSON2 _ t _ _ _ _ (Const x) = t x
    liftToEncoding2 _ t _ _ _ _ (Const x) = t x
    liftOmitField2 o _ (Const x) = o x

instance ToJSON a => ToJSON1 (Const a) where
    liftToJSON _ _ _ (Const x) = toJSON x
    liftToEncoding _ _ _ (Const x) = toEncoding x
    liftOmitField _ (Const x) = omitField x

instance ToJSON a => ToJSON (Const a b) where
    toJSON (Const x) = toJSON x
    toEncoding (Const x) = toEncoding x
    omitField (Const x) = omitField x

instance (ToJSON a, ToJSONKey a) => ToJSONKey (Const a b) where
    toJSONKey = contramap getConst toJSONKey


instance ToJSON1 Maybe where
    liftToJSON _ t _ (Just a) = t a
    liftToJSON _ _  _ Nothing  = Null

    liftToEncoding _ t _ (Just a) = t a
    liftToEncoding _ _  _ Nothing  = E.null_

    liftOmitField _ = isNothing

instance (ToJSON a) => ToJSON (Maybe a) where
    toJSON = toJSON1
    omitField = omitField1
    toEncoding = toEncoding1


instance ToJSON2 Either where
    liftToJSON2 _  toA _ _ _toB _ (Left a)  = Object $ KM.singleton "Left"  (toA a)
    liftToJSON2 _ _toA _ _  toB _ (Right b) = Object $ KM.singleton "Right" (toB b)

    liftToEncoding2 _  toA _ _ _toB _ (Left a) = E.pairs $ E.pair "Left" $ toA a
    liftToEncoding2 _ _toA _ _ toB _ (Right b) = E.pairs $ E.pair "Right" $ toB b

instance (ToJSON a) => ToJSON1 (Either a) where
    liftToJSON = liftToJSON2 omitField toJSON toJSONList
    liftToEncoding = liftToEncoding2 omitField toEncoding toEncodingList

instance (ToJSON a, ToJSON b) => ToJSON (Either a b) where
    toJSON = toJSON2
    toEncoding = toEncoding2

instance ToJSON Void where
    toJSON = absurd
    toEncoding = absurd

-- | @since 2.1.2.0
instance ToJSONKey Void where
    toJSONKey = ToJSONKeyText absurd absurd

instance ToJSON Bool where
    toJSON = Bool
    toEncoding = E.bool

instance ToJSONKey Bool where
    toJSONKey = toJSONKeyText $ \x -> if x then "true" else "false"


instance ToJSON Ordering where
  toJSON     = toJSON     . orderingToText
  toEncoding = toEncoding . orderingToText

orderingToText :: Ordering -> T.Text
orderingToText o = case o of
                     LT -> "LT"
                     EQ -> "EQ"
                     GT -> "GT"

instance ToJSON () where
    toJSON _ = emptyArray
    toEncoding _ = emptyArray_
    omitField _ = True


instance ToJSON Char where
    toJSON = String . T.singleton
    toJSONList = String . T.pack

    toEncoding = E.string . (:[])
    toEncodingList = E.string


instance ToJSON Double where
    toJSON = realFloatToJSON
    toEncoding = E.double

instance ToJSONKey Double where
    toJSONKey = toJSONKeyTextEnc E.doubleText

instance ToJSON Float where
    toJSON = realFloatToJSON
    toEncoding = E.float

instance ToJSONKey Float where
    toJSONKey = toJSONKeyTextEnc E.floatText


instance (ToJSON a, Integral a) => ToJSON (Ratio a) where
    toJSON r = object [ "numerator"   .= numerator   r
                      , "denominator" .= denominator r
                      ]

    toEncoding r = E.pairs $
        "numerator" .= numerator r <>
        "denominator" .= denominator r


instance HasResolution a => ToJSON (Fixed a) where
    toJSON = Number . realToFrac
    toEncoding = E.scientific . realToFrac

instance HasResolution a => ToJSONKey (Fixed a) where
    toJSONKey = toJSONKeyTextEnc (E.scientificText . realToFrac)

instance ToJSON Int where
    toJSON = Number . fromIntegral
    toEncoding = E.int

instance ToJSONKey Int where
    toJSONKey = toJSONKeyTextEnc E.intText


instance ToJSON Integer where
    toJSON = Number . fromInteger
    toEncoding = E.integer

instance ToJSONKey Integer where
    toJSONKey = toJSONKeyTextEnc E.integerText


instance ToJSON Natural where
    toJSON = toJSON . toInteger
    toEncoding = toEncoding . toInteger

instance ToJSONKey Natural where
    toJSONKey = toJSONKeyTextEnc (E.integerText . toInteger)


instance ToJSON Int8 where
    toJSON = Number . fromIntegral
    toEncoding = E.int8

instance ToJSONKey Int8 where
    toJSONKey = toJSONKeyTextEnc E.int8Text


instance ToJSON Int16 where
    toJSON = Number . fromIntegral
    toEncoding = E.int16

instance ToJSONKey Int16 where
    toJSONKey = toJSONKeyTextEnc E.int16Text


instance ToJSON Int32 where
    toJSON = Number . fromIntegral
    toEncoding = E.int32

instance ToJSONKey Int32 where
    toJSONKey = toJSONKeyTextEnc E.int32Text


instance ToJSON Int64 where
    toJSON = Number . fromIntegral
    toEncoding = E.int64

instance ToJSONKey Int64 where
    toJSONKey = toJSONKeyTextEnc E.int64Text

instance ToJSON Word where
    toJSON = Number . fromIntegral
    toEncoding = E.word

instance ToJSONKey Word where
    toJSONKey = toJSONKeyTextEnc E.wordText


instance ToJSON Word8 where
    toJSON = Number . fromIntegral
    toEncoding = E.word8

instance ToJSONKey Word8 where
    toJSONKey = toJSONKeyTextEnc E.word8Text


instance ToJSON Word16 where
    toJSON = Number . fromIntegral
    toEncoding = E.word16

instance ToJSONKey Word16 where
    toJSONKey = toJSONKeyTextEnc E.word16Text


instance ToJSON Word32 where
    toJSON = Number . fromIntegral
    toEncoding = E.word32

instance ToJSONKey Word32 where
    toJSONKey = toJSONKeyTextEnc E.word32Text


instance ToJSON Word64 where
    toJSON = Number . fromIntegral
    toEncoding = E.word64

instance ToJSONKey Word64 where
    toJSONKey = toJSONKeyTextEnc E.word64Text

instance ToJSON CTime where
    toJSON (CTime i) = toJSON i
    toEncoding (CTime i) = toEncoding i

instance ToJSON Text where
    toJSON = String
    toEncoding = E.text

instance ToJSONKey Text where
    toJSONKey = toJSONKeyText id

instance ToJSON LT.Text where
    toJSON = String . LT.toStrict
    toEncoding = E.lazyText

instance ToJSONKey LT.Text where
    toJSONKey = toJSONKeyText LT.toStrict

-- | @since 2.0.2.0
instance ToJSON ST.ShortText where
    toJSON = String . ST.toText
    toEncoding = E.shortText

-- | @since 2.0.2.0
instance ToJSONKey ST.ShortText where
    toJSONKey = ToJSONKeyText Key.fromShortText E.shortText


instance ToJSON Version where
    toJSON = toJSON . showVersion
    toEncoding = toEncoding . showVersion

instance ToJSONKey Version where
    toJSONKey = toJSONKeyKey (Key.fromString . showVersion)

-------------------------------------------------------------------------------
-- semigroups NonEmpty
-------------------------------------------------------------------------------

instance ToJSON1 NonEmpty where
    liftToJSON _ t _ = listValue t . NE.toList
    liftToEncoding _ t _ = listEncoding t . NE.toList

instance (ToJSON a) => ToJSON (NonEmpty a) where
    toJSON = toJSON1
    toEncoding = toEncoding1

-------------------------------------------------------------------------------
-- scientific
-------------------------------------------------------------------------------

instance ToJSON Scientific where
    toJSON = Number
    toEncoding = E.scientific

instance ToJSONKey Scientific where
    toJSONKey = toJSONKeyTextEnc E.scientificText

-------------------------------------------------------------------------------
-- DList
-------------------------------------------------------------------------------

instance ToJSON1 DList.DList where
    liftToJSON _ t _ = listValue t . toList
    liftToEncoding _ t _ = listEncoding t . toList

instance (ToJSON a) => ToJSON (DList.DList a) where
    toJSON = toJSON1
    toEncoding = toEncoding1

-- | @since 1.5.3.0
instance ToJSON1 DNE.DNonEmpty where
    liftToJSON _ t _ = listValue t . DNE.toList
    liftToEncoding _ t _ = listEncoding t . DNE.toList

-- | @since 1.5.3.0
instance (ToJSON a) => ToJSON (DNE.DNonEmpty a) where
    toJSON = toJSON1
    toEncoding = toEncoding1

-------------------------------------------------------------------------------
-- OneTuple
-------------------------------------------------------------------------------

-- | @since 2.0.2.0
instance ToJSON1 Solo where
    liftToJSON _ t _ (Solo a) = t a
    liftToJSONList _ _ tl xs = tl (map getSolo xs)

    liftToEncoding _ t _ (Solo a) = t a
    liftToEncodingList _ _ tl xs = tl (map getSolo xs)

-- | @since 2.0.2.0
instance (ToJSON a) => ToJSON (Solo a) where
    toJSON = toJSON1
    toJSONList = liftToJSONList omitField toJSON toJSONList

    toEncoding = toEncoding1
    toEncodingList = liftToEncodingList omitField toEncoding toEncodingList

-- | @since 2.0.2.0
instance (ToJSONKey a) => ToJSONKey (Solo a) where
    toJSONKey = contramapToJSONKeyFunction getSolo toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (map getSolo) toJSONKeyList

-------------------------------------------------------------------------------
-- transformers - Functors
-------------------------------------------------------------------------------

instance ToJSON1 Identity where
    liftToJSON _ t _ (Identity a) = t a
    liftToJSONList _ _ tl xs = tl (map runIdentity xs)

    liftToEncoding _ t _ (Identity a) = t a
    liftToEncodingList _ _ tl xs = tl (map runIdentity xs)

    liftOmitField o (Identity a) = o a

instance (ToJSON a) => ToJSON (Identity a) where
    toJSON = toJSON1
    toJSONList = liftToJSONList omitField toJSON toJSONList

    toEncoding = toEncoding1
    toEncodingList = liftToEncodingList omitField toEncoding toEncodingList

    omitField (Identity x) = omitField x

instance (ToJSONKey a) => ToJSONKey (Identity a) where
    toJSONKey = contramapToJSONKeyFunction runIdentity toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (map runIdentity) toJSONKeyList


instance (ToJSON1 f, ToJSON1 g) => ToJSON1 (Compose f g) where
    liftToJSON o tv tvl (Compose x) = liftToJSON (liftOmitField o) g gl x
      where
        g = liftToJSON o tv tvl
        gl = liftToJSONList o tv tvl

    liftToJSONList o te tel xs = liftToJSONList (liftOmitField o) g gl (map getCompose xs)
      where
        g = liftToJSON o te tel
        gl = liftToJSONList o te tel

    liftToEncoding o te tel (Compose x) = liftToEncoding (liftOmitField o) g gl x
      where
        g = liftToEncoding o te tel
        gl = liftToEncodingList o te tel

    liftToEncodingList o te tel xs = liftToEncodingList (liftOmitField o) g gl (map getCompose xs)
      where
        g = liftToEncoding o te tel
        gl = liftToEncodingList o te tel

    liftOmitField o (Compose xs)= liftOmitField (liftOmitField o) xs

instance (ToJSON1 f, ToJSON1 g, ToJSON a) => ToJSON (Compose f g a) where
    toJSON = toJSON1
    toJSONList = liftToJSONList omitField toJSON toJSONList
    toEncoding = toEncoding1
    toEncodingList = liftToEncodingList omitField toEncoding toEncodingList
    omitField = omitField1

instance (ToJSON1 f, ToJSON1 g) => ToJSON1 (Product f g) where
    liftToJSON o tv tvl (Pair x y) = liftToJSON2 (liftOmitField o) tx txl (liftOmitField o) ty tyl (x, y)
      where
        tx = liftToJSON o tv tvl
        txl = liftToJSONList o tv tvl
        ty = liftToJSON o tv tvl
        tyl = liftToJSONList o tv tvl

    liftToEncoding o te tel (Pair x y) = liftToEncoding2 (liftOmitField o) tx txl (liftOmitField o) ty tyl (x, y)
      where
        tx = liftToEncoding o te tel
        txl = liftToEncodingList o te tel
        ty = liftToEncoding o te tel
        tyl = liftToEncodingList o te tel

instance (ToJSON1 f, ToJSON1 g, ToJSON a) => ToJSON (Product f g a) where
    toJSON = toJSON1
    toEncoding = toEncoding1

instance (ToJSON1 f, ToJSON1 g) => ToJSON1 (Sum f g) where
    liftToJSON o tv tvl (InL x) = Object $ KM.singleton "InL" (liftToJSON o tv tvl x)
    liftToJSON o tv tvl (InR y) = Object $ KM.singleton "InR" (liftToJSON o tv tvl y)

    liftToEncoding o te tel (InL x) = E.pairs $ E.pair "InL" $ liftToEncoding o te tel x
    liftToEncoding o te tel (InR y) = E.pairs $ E.pair "InR" $ liftToEncoding o te tel y

instance (ToJSON1 f, ToJSON1 g, ToJSON a) => ToJSON (Sum f g a) where
    toJSON = toJSON1
    toEncoding = toEncoding1

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance ToJSON1 Seq.Seq where
    liftToJSON _ t _ = listValue t . toList
    liftToEncoding _ t _ = listEncoding t . toList

instance (ToJSON a) => ToJSON (Seq.Seq a) where
    toJSON = toJSON1
    toEncoding = toEncoding1


instance ToJSON1 Set.Set where
    liftToJSON _ t _ = listValue t . Set.toList
    liftToEncoding _ t _ = listEncoding t . Set.toList

instance (ToJSON a) => ToJSON (Set.Set a) where
    toJSON = toJSON1
    toEncoding = toEncoding1


instance ToJSON IntSet.IntSet where
    toJSON = toJSON . IntSet.toList
    toEncoding = toEncoding . IntSet.toList

instance ToJSON1 IntMap.IntMap where
    liftToJSON o t tol = liftToJSON (liftOmitField o) to' tol' . IntMap.toList
      where
        to'  = liftToJSON2     omitField toJSON toJSONList o t tol
        tol' = liftToJSONList2 omitField toJSON toJSONList o t tol

    liftToEncoding o t tol = liftToEncoding (liftOmitField o) to' tol' . IntMap.toList
      where
        to'  = liftToEncoding2     omitField toEncoding toEncodingList o t tol
        tol' = liftToEncodingList2 omitField toEncoding toEncodingList o t tol

instance ToJSON a => ToJSON (IntMap.IntMap a) where
    toJSON = toJSON1
    toEncoding = toEncoding1


instance ToJSONKey k => ToJSON1 (M.Map k) where
    liftToJSON _ g _ = case toJSONKey of
        ToJSONKeyText f _ -> Object . KM.fromMap . mapKeyValO f g
        ToJSONKeyValue  f _ -> Array . V.fromList . map (toJSONPair f g) . M.toList

    liftToEncoding _ g _ = case toJSONKey of
        ToJSONKeyText _ f -> dict f g M.foldrWithKey
        ToJSONKeyValue _ f -> listEncoding (pairEncoding f) . M.toList
      where
        pairEncoding f (a, b) = E.list id [f a, g b]


instance (ToJSON v, ToJSONKey k) => ToJSON (M.Map k v) where
    toJSON = toJSON1
    toEncoding = toEncoding1


instance ToJSON1 Tree.Tree where
    liftToJSON o t tol = go
      where
        go (Tree.Node root branches) =
            liftToJSON2 o t tol (const False) to' tol' (root, branches)

        to' = liftToJSON (const False) go (listValue go)
        tol' = liftToJSONList (const False) go (listValue go)

    liftToEncoding o t tol = go
      where
        go (Tree.Node root branches) =
            liftToEncoding2 o t tol (const False) to' tol' (root, branches)

        to' = liftToEncoding (const False) go (listEncoding go)
        tol' = liftToEncodingList (const False) go (listEncoding go)

instance (ToJSON v) => ToJSON (Tree.Tree v) where
    toJSON = toJSON1
    toEncoding = toEncoding1

-------------------------------------------------------------------------------
-- uuid
-------------------------------------------------------------------------------

instance ToJSON UUID.UUID where
    toJSON = toJSON . UUID.toText
    toEncoding = E.unsafeToEncoding . EB.quote . B.byteString . UUID.toASCIIBytes

instance ToJSONKey UUID.UUID where
    toJSONKey = ToJSONKeyText (Key.fromText . UUID.toText) $
        E.unsafeToEncoding . EB.quote . B.byteString . UUID.toASCIIBytes

-------------------------------------------------------------------------------
-- vector
-------------------------------------------------------------------------------

instance ToJSON1 Vector where
    liftToJSON _ t _ = Array . V.map t
    liftToEncoding _ t _ =  listEncoding t . V.toList

instance (ToJSON a) => ToJSON (Vector a) where
    {-# SPECIALIZE instance ToJSON Array #-}

    toJSON = toJSON1
    toEncoding = toEncoding1

encodeVector :: (ToJSON a, VG.Vector v a) => v a -> Encoding
encodeVector = listEncoding toEncoding . VG.toList
{-# INLINE encodeVector #-}

vectorToJSON :: (VG.Vector v a, ToJSON a) => v a -> Value
vectorToJSON = Array . V.map toJSON . V.convert
{-# INLINE vectorToJSON #-}

instance (Storable a, ToJSON a) => ToJSON (VS.Vector a) where
    toJSON = vectorToJSON
    toEncoding = encodeVector


instance (VP.Prim a, ToJSON a) => ToJSON (VP.Vector a) where
    toJSON = vectorToJSON
    toEncoding = encodeVector


instance (VG.Vector VU.Vector a, ToJSON a) => ToJSON (VU.Vector a) where
    toJSON = vectorToJSON
    toEncoding = encodeVector

-------------------------------------------------------------------------------
-- unordered-containers
-------------------------------------------------------------------------------

instance ToJSON1 HashSet.HashSet where
    liftToJSON _ t _ = listValue t . HashSet.toList
    liftToEncoding _ t _ = listEncoding t . HashSet.toList

instance (ToJSON a) => ToJSON (HashSet.HashSet a) where
    toJSON = toJSON1
    toEncoding = toEncoding1


instance ToJSONKey k => ToJSON1 (H.HashMap k) where
    liftToJSON _ g _ = case toJSONKey of
        ToJSONKeyText f _ -> Object . KM.fromHashMap . mapKeyVal f g
        ToJSONKeyValue f _
          -> Array . V.fromList . map (toJSONPair f g) . H.toList

    -- liftToEncoding :: forall a. (a -> Encoding) -> ([a] -> Encoding) -> KM.HashMap k a -> Encoding
    liftToEncoding _ g _ = case toJSONKey of
        ToJSONKeyText _ f -> dict f g H.foldrWithKey
        ToJSONKeyValue _ f -> listEncoding (pairEncoding f) . H.toList
      where
        pairEncoding f (a, b) = E.list id [f a, g b]

instance (ToJSON v, ToJSONKey k) => ToJSON (H.HashMap k v) where
    toJSON = toJSON1
    toEncoding = toEncoding1

-------------------------------------------------------------------------------
-- Data.Aeson.KeyMap
-------------------------------------------------------------------------------

instance ToJSON1 KM.KeyMap where
    liftToJSON _ g _ = Object . fmap g
    liftToEncoding _ g _ = dict E.key g KM.foldrWithKey

instance (ToJSON v) => ToJSON (KM.KeyMap v) where
    {-# SPECIALIZE instance ToJSON Object #-}

    toJSON = toJSON1
    toEncoding = toEncoding1

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance ToJSON Key where
    toJSON = toJSON . Key.toText
    toEncoding = E.key

instance ToJSONKey Key where
    toJSONKey = ToJSONKeyText id E.key

instance ToJSON Value where
    toJSON a = a
    toEncoding = E.value

instance ToJSON DotNetTime where
    toJSON = toJSON . dotNetTime

    toEncoding = toEncoding . dotNetTime

dotNetTime :: DotNetTime -> String
dotNetTime (DotNetTime t) = secs ++ formatMillis t ++ ")/"
  where secs  = formatTime defaultTimeLocale "/Date(%s" t

formatMillis :: (FormatTime t) => t -> String
formatMillis = take 3 . formatTime defaultTimeLocale "%q"

-------------------------------------------------------------------------------
-- primitive
-------------------------------------------------------------------------------

instance ToJSON a => ToJSON (PM.Array a) where
  -- note: we could do better than this if vector exposed the data
  -- constructor in Data.Vector.
  toJSON = toJSON . Exts.toList
  toEncoding = toEncoding . Exts.toList

instance ToJSON a => ToJSON (PM.SmallArray a) where
  toJSON = toJSON . Exts.toList
  toEncoding = toEncoding . Exts.toList

instance (PM.Prim a,ToJSON a) => ToJSON (PM.PrimArray a) where
  toJSON = toJSON . Exts.toList
  toEncoding = toEncoding . Exts.toList

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

instance ToJSON Day where
    toJSON     = stringEncoding . E.day
    toEncoding = E.day

instance ToJSONKey Day where
    toJSONKey = toJSONKeyTextEnc E.day

instance ToJSON Month where
    toJSON     = stringEncoding . E.month
    toEncoding = E.month

instance ToJSONKey Month where
    toJSONKey = toJSONKeyTextEnc E.month

instance ToJSON Quarter where
    toJSON     = stringEncoding . E.quarter
    toEncoding = E.quarter

instance ToJSONKey Quarter where
    toJSONKey = toJSONKeyTextEnc E.quarter

instance ToJSON TimeOfDay where
    toJSON     = stringEncoding . E.timeOfDay
    toEncoding = E.timeOfDay

instance ToJSONKey TimeOfDay where
    toJSONKey = toJSONKeyTextEnc E.timeOfDay


instance ToJSON LocalTime where
    toJSON     = stringEncoding . E.localTime
    toEncoding = E.localTime

instance ToJSONKey LocalTime where
    toJSONKey = toJSONKeyTextEnc E.localTime


instance ToJSON ZonedTime where
    toJSON     = stringEncoding . E.zonedTime
    toEncoding = E.zonedTime

instance ToJSONKey ZonedTime where
    toJSONKey = toJSONKeyTextEnc E.zonedTime


instance ToJSON UTCTime where
    toJSON     = stringEncoding . E.utcTime
    toEncoding = E.utcTime

instance ToJSONKey UTCTime where
    toJSONKey = toJSONKeyTextEnc E.utcTime

-- | Encode something t a JSON string.
stringEncoding :: Encoding' Text -> Value
stringEncoding = String
    . T.dropAround (== '"')
    . T.decodeLatin1
    . L.toStrict
    . E.encodingToLazyByteString
{-# INLINE stringEncoding #-}


instance ToJSON NominalDiffTime where
    toJSON = Number . realToFrac
    toEncoding = E.scientific . realToFrac


instance ToJSON DiffTime where
    toJSON = Number . realToFrac
    toEncoding = E.scientific . realToFrac

-- | Encoded as number
instance ToJSON SystemTime where
    toJSON (MkSystemTime secs nsecs) =
        toJSON (fromIntegral secs + fromIntegral nsecs / 1000000000 :: Nano)
    toEncoding (MkSystemTime secs nsecs) =
        toEncoding (fromIntegral secs + fromIntegral nsecs / 1000000000 :: Nano)

instance ToJSON CalendarDiffTime where
    toJSON (CalendarDiffTime m nt) = object
        [ "months" .= m
        , "time" .= nt
        ]
    toEncoding (CalendarDiffTime m nt) = E.pairs
        ("months" .= m <> "time" .= nt)

instance ToJSON CalendarDiffDays where
    toJSON (CalendarDiffDays m d) = object
        [ "months" .= m
        , "days" .= d
        ]
    toEncoding (CalendarDiffDays m d) = E.pairs
        ("months" .= m <> "days" .= d)

instance ToJSON DayOfWeek where
    toJSON Monday    = "monday"
    toJSON Tuesday   = "tuesday"
    toJSON Wednesday = "wednesday"
    toJSON Thursday  = "thursday"
    toJSON Friday    = "friday"
    toJSON Saturday  = "saturday"
    toJSON Sunday    = "sunday"

    toEncoding = toEncodingDayOfWeek

toEncodingDayOfWeek :: DayOfWeek -> E.Encoding' a
toEncodingDayOfWeek Monday    = E.unsafeToEncoding "\"monday\""
toEncodingDayOfWeek Tuesday   = E.unsafeToEncoding "\"tuesday\""
toEncodingDayOfWeek Wednesday = E.unsafeToEncoding "\"wednesday\""
toEncodingDayOfWeek Thursday  = E.unsafeToEncoding "\"thursday\""
toEncodingDayOfWeek Friday    = E.unsafeToEncoding "\"friday\""
toEncodingDayOfWeek Saturday  = E.unsafeToEncoding "\"saturday\""
toEncodingDayOfWeek Sunday    = E.unsafeToEncoding "\"sunday\""

instance ToJSONKey DayOfWeek where
    toJSONKey = toJSONKeyTextEnc toEncodingDayOfWeek

instance ToJSON QuarterOfYear where
    toJSON Q1 = "q1"
    toJSON Q2 = "q2"
    toJSON Q3 = "q3"
    toJSON Q4 = "q4"

    toEncoding = toEncodingQuarterOfYear

toEncodingQuarterOfYear :: QuarterOfYear -> E.Encoding' a
toEncodingQuarterOfYear Q1 = E.unsafeToEncoding "\"q1\""
toEncodingQuarterOfYear Q2 = E.unsafeToEncoding "\"q2\""
toEncodingQuarterOfYear Q3 = E.unsafeToEncoding "\"q3\""
toEncodingQuarterOfYear Q4 = E.unsafeToEncoding "\"q4\""

instance ToJSONKey QuarterOfYear where
    toJSONKey = toJSONKeyTextEnc toEncodingQuarterOfYear

-------------------------------------------------------------------------------
-- base Down
-------------------------------------------------------------------------------

-- | @since 2.2.0.0
instance ToJSON1 Down where
    liftToJSON _ t _ = coerce t
    liftToEncoding _ t _ = coerce t
    liftOmitField = coerce

-- | @since 2.2.0.0
instance ToJSON a => ToJSON (Down a) where
    toJSON = toJSON1
    toEncoding = toEncoding1
    omitField = omitField1

-------------------------------------------------------------------------------
-- base Monoid/Semigroup
-------------------------------------------------------------------------------

instance ToJSON1 Monoid.Dual where
    liftToJSON _ t _ = t . Monoid.getDual
    liftToEncoding _ t _ = t . Monoid.getDual
    liftOmitField = coerce

instance ToJSON a => ToJSON (Monoid.Dual a) where
    toJSON = toJSON1
    toEncoding = toEncoding1
    omitField = omitField1

instance ToJSON1 Monoid.First where
    liftToJSON o t to' = liftToJSON o t to' . Monoid.getFirst
    liftToEncoding o t to' = liftToEncoding o t to' . Monoid.getFirst
    liftOmitField :: forall a. (a -> Bool) -> Monoid.First a -> Bool
    liftOmitField _ = coerce (isNothing @a)
    
instance ToJSON a => ToJSON (Monoid.First a) where
    toJSON = toJSON1
    toEncoding = toEncoding1
    omitField = omitField1

instance ToJSON1 Monoid.Last where
    liftToJSON o t to' = liftToJSON o t to' . Monoid.getLast
    liftToEncoding o t to' = liftToEncoding o t to' . Monoid.getLast

    liftOmitField :: forall a. (a -> Bool) -> Monoid.Last a -> Bool
    liftOmitField _ = coerce (isNothing @a)

instance ToJSON a => ToJSON (Monoid.Last a) where
    toJSON = toJSON1
    toEncoding = toEncoding1
    omitField = omitField1

instance ToJSON1 Semigroup.Min where
    liftToJSON _ t _ (Semigroup.Min x) = t x
    liftToEncoding _ t _ (Semigroup.Min x) = t x
    liftOmitField = coerce

instance ToJSON a => ToJSON (Semigroup.Min a) where
    toJSON = toJSON1
    toEncoding = toEncoding1
    omitField = omitField1


instance ToJSON1 Semigroup.Max where
    liftToJSON _ t _ (Semigroup.Max x) = t x
    liftToEncoding _ t _ (Semigroup.Max x) = t x
    liftOmitField = coerce

instance ToJSON a => ToJSON (Semigroup.Max a) where
    toJSON = toJSON1
    toEncoding = toEncoding1
    omitField = omitField1

instance ToJSON1 Semigroup.First where
    liftToJSON _ t _ (Semigroup.First x) = t x
    liftToEncoding _ t _ (Semigroup.First x) = t x
    liftOmitField = coerce

instance ToJSON a => ToJSON (Semigroup.First a) where
    toJSON = toJSON1
    toEncoding = toEncoding1
    omitField = omitField1

instance ToJSON1 Semigroup.Last where
    liftToJSON _ t _ (Semigroup.Last x) = t x
    liftToEncoding _ t _ (Semigroup.Last x) = t x
    liftOmitField = coerce

instance ToJSON a => ToJSON (Semigroup.Last a) where
    toJSON = toJSON1
    toEncoding = toEncoding1
    omitField = omitField1

instance ToJSON1 Semigroup.WrappedMonoid where
    liftToJSON _ t _ (Semigroup.WrapMonoid x) = t x
    liftToEncoding _ t _ (Semigroup.WrapMonoid x) = t x
    liftOmitField = coerce
    
instance ToJSON a => ToJSON (Semigroup.WrappedMonoid a) where
    toJSON = toJSON1
    toEncoding = toEncoding1
    omitField = omitField1

#if !MIN_VERSION_base(4,16,0)
instance ToJSON1 Semigroup.Option where
    liftToJSON o t to' = liftToJSON o t to' . Semigroup.getOption
    liftToEncoding o t to' = liftToEncoding o t to' . Semigroup.getOption
    liftOmitField _ = isNothing . Semigroup.getOption

instance ToJSON a => ToJSON (Semigroup.Option a) where
    toJSON = toJSON1
    toEncoding = toEncoding1
    omitField = omitField1
#endif

-------------------------------------------------------------------------------
-- data-fix
-------------------------------------------------------------------------------

-- | @since 1.5.3.0
instance ToJSON1 f => ToJSON (F.Fix f) where
    toJSON     = go where go (F.Fix f) = liftToJSON omitField go toJSONList f
    toEncoding = go where go (F.Fix f) = liftToEncoding omitField go toEncodingList f
    omitField  = go where go (F.Fix f) = liftOmitField go f

-- | @since 1.5.3.0
instance (ToJSON1 f, Functor f) => ToJSON (F.Mu f) where
    toJSON     = F.foldMu (liftToJSON (const False) id (listValue id))
    toEncoding = F.foldMu (liftToEncoding (const False) id (listEncoding id))

-- | @since 1.5.3.0
instance (ToJSON1 f, Functor f) => ToJSON (F.Nu f) where
    toJSON     = F.foldNu (liftToJSON (const False) id (listValue id))
    toEncoding = F.foldNu (liftToEncoding (const False) id (listEncoding id))

-------------------------------------------------------------------------------
-- network-uri
-------------------------------------------------------------------------------

-- | @since 2.2.0.0
instance ToJSON URI.URI where
    toJSON uri = toJSON (URI.uriToString id uri "")
    toEncoding = encodeURI

-- | @since 2.2.0.0
instance ToJSONKey URI.URI where
    toJSONKey = toJSONKeyTextEnc encodeURI

encodeURI :: URI.URI -> Encoding' a
encodeURI uri = E.string (URI.uriToString id uri "")

-------------------------------------------------------------------------------
-- strict
-------------------------------------------------------------------------------

-- | @since 1.5.3.0
instance (ToJSON a, ToJSON b) => ToJSON (S.These a b) where
    toJSON = toJSON . S.toLazy
    toEncoding = toEncoding . S.toLazy

-- | @since 1.5.3.0
instance ToJSON2 S.These where
    liftToJSON2 oa toa toas ob tob tobs = liftToJSON2 oa toa toas ob tob tobs . S.toLazy
    liftToEncoding2 oa toa toas ob tob tobs = liftToEncoding2 oa toa toas ob tob tobs . S.toLazy

-- | @since 1.5.3.0
instance ToJSON a => ToJSON1 (S.These a) where
    liftToJSON oa toa tos = liftToJSON oa toa tos . S.toLazy
    liftToEncoding oa toa tos = liftToEncoding oa toa tos . S.toLazy

-- | @since 1.5.3.0
instance (ToJSON a, ToJSON b) => ToJSON (S.Pair a b) where
    toJSON = toJSON . S.toLazy
    toEncoding = toEncoding . S.toLazy

-- | @since 1.5.3.0
instance ToJSON2 S.Pair where
    liftToJSON2 oa toa toas ob tob tobs = liftToJSON2 oa toa toas ob tob tobs . S.toLazy
    liftToEncoding2 oa toa toas ob tob tobs = liftToEncoding2 oa toa toas ob tob tobs . S.toLazy

-- | @since 1.5.3.0
instance ToJSON a => ToJSON1 (S.Pair a) where
    liftToJSON oa toa tos = liftToJSON oa toa tos . S.toLazy
    liftToEncoding oa toa tos = liftToEncoding oa toa tos . S.toLazy

-- | @since 1.5.3.0
instance (ToJSON a, ToJSON b) => ToJSON (S.Either a b) where
    toJSON = toJSON . S.toLazy
    toEncoding = toEncoding . S.toLazy

-- | @since 1.5.3.0
instance ToJSON2 S.Either where
    liftToJSON2 oa toa toas ob tob tobs = liftToJSON2 oa toa toas ob tob tobs . S.toLazy
    liftToEncoding2 oa toa toas ob tob tobs = liftToEncoding2 oa toa toas ob tob tobs . S.toLazy

-- | @since 1.5.3.0
instance ToJSON a => ToJSON1 (S.Either a) where
    liftToJSON oa toa tos = liftToJSON oa toa tos . S.toLazy
    liftToEncoding oa toa tos = liftToEncoding oa toa tos . S.toLazy

-- | @since 1.5.3.0
instance ToJSON a => ToJSON (S.Maybe a) where
    toJSON = toJSON . S.toLazy
    toEncoding = toEncoding . S.toLazy
    omitField = omitField . S.toLazy

-- | @since 1.5.3.0
instance ToJSON1 S.Maybe where
    liftToJSON oa toa tos = liftToJSON oa toa tos . S.toLazy
    liftToEncoding oa toa tos = liftToEncoding oa toa tos . S.toLazy
    liftOmitField oa = liftOmitField oa . S.toLazy

-------------------------------------------------------------------------------
-- tagged
-------------------------------------------------------------------------------

instance ToJSON1 Proxy where
    liftToJSON _ _ _ _ = Null
    liftToEncoding _ _ _ _ = E.null_
    liftOmitField _ _ = True

instance ToJSON (Proxy a) where
    toJSON _ = Null
    toEncoding _ = E.null_
    omitField _ = True

instance ToJSON2 Tagged where
    liftToJSON2 _ _ _ _ t _ (Tagged x) = t x
    liftToEncoding2 _ _ _ _ t _ (Tagged x) = t x
    liftOmitField2 _ = coerce

instance ToJSON1 (Tagged a) where
    liftToJSON _ t _ (Tagged x) = t x
    liftToEncoding _ t _ (Tagged x) = t x
    liftOmitField = coerce

instance ToJSON b => ToJSON (Tagged a b) where
    toJSON = toJSON1
    toEncoding = toEncoding1
    omitField = coerce (omitField @b)

instance ToJSONKey b => ToJSONKey (Tagged a b) where
    toJSONKey = contramapToJSONKeyFunction unTagged toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (fmap unTagged) toJSONKeyList

-------------------------------------------------------------------------------
-- these
-------------------------------------------------------------------------------

-- | @since 1.5.1.0
instance (ToJSON a, ToJSON b) => ToJSON (These a b) where
    toJSON (This a)    = object [ "This" .= a ]
    toJSON (That b)    = object [ "That" .= b ]
    toJSON (These a b) = object [ "This" .= a, "That" .= b ]

    toEncoding (This a)    = E.pairs $ "This" .= a
    toEncoding (That b)    = E.pairs $ "That" .= b
    toEncoding (These a b) = E.pairs $ "This" .= a <> "That" .= b

-- | @since 1.5.1.0
instance ToJSON2 These where
    liftToJSON2 _  toa _ _ _tob _ (This a)    = object [ "This" .= toa a ]
    liftToJSON2 _ _toa _ _  tob _ (That b)    = object [ "That" .= tob b ]
    liftToJSON2 _  toa _ _  tob _ (These a b) = object [ "This" .= toa a, "That" .= tob b ]

    liftToEncoding2 _  toa _ _ _tob _ (This a)    = E.pairs $ E.pair "This" (toa a)
    liftToEncoding2 _ _toa _ _  tob _ (That b)    = E.pairs $ E.pair "That" (tob b)
    liftToEncoding2 _  toa _ _  tob _ (These a b) = E.pairs $ E.pair "This" (toa a) <> E.pair "That" (tob b)

-- | @since 1.5.1.0
instance ToJSON a => ToJSON1 (These a) where
    liftToJSON _ _tob _ (This a)    = object [ "This" .= a ]
    liftToJSON _  tob _ (That b)    = object [ "That" .= tob b ]
    liftToJSON _  tob _ (These a b) = object [ "This" .= a, "That" .= tob b ]

    liftToEncoding _ _tob _ (This a)    = E.pairs $ "This" .= a
    liftToEncoding _  tob _ (That b)    = E.pairs $ E.pair "That" (tob b)
    liftToEncoding _  tob _ (These a b) = E.pairs $ "This" .= a <> E.pair "That" (tob b)

-- | @since 1.5.1.0
instance (ToJSON1 f, ToJSON1 g) => ToJSON1 (These1 f g) where
    liftToJSON o tx tl (This1 a)    = object [ "This" .= liftToJSON o tx tl a ]
    liftToJSON o tx tl (That1 b)    = object [ "That" .= liftToJSON o tx tl b ]
    liftToJSON o tx tl (These1 a b) = object [ "This" .= liftToJSON o tx tl a, "That" .= liftToJSON o tx tl b ]

    liftToEncoding o tx tl (This1 a)    = E.pairs $ E.pair "This" (liftToEncoding o tx tl a)
    liftToEncoding o tx tl (That1 b)    = E.pairs $ E.pair "That" (liftToEncoding o tx tl b)
    liftToEncoding o tx tl (These1 a b) = E.pairs $
        pair "This" (liftToEncoding o tx tl a) `mappend`
        pair "That" (liftToEncoding o tx tl b)

-- | @since 1.5.1.0
instance (ToJSON1 f, ToJSON1 g, ToJSON a) => ToJSON (These1 f g a) where
    toJSON     = toJSON1
    toEncoding = toEncoding1

-------------------------------------------------------------------------------
-- Instances for converting t map keys
-------------------------------------------------------------------------------

instance (ToJSON a, ToJSON b) => ToJSONKey (a,b)
instance (ToJSON a, ToJSON b, ToJSON c) => ToJSONKey (a,b,c)
instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSONKey (a,b,c,d)

instance ToJSONKey Char where
    toJSONKey = toJSONKeyText T.singleton
    toJSONKeyList = toJSONKeyText T.pack

instance (ToJSONKey a, ToJSON a) => ToJSONKey [a] where
    toJSONKey = toJSONKeyList

-------------------------------------------------------------------------------
-- Tuple instances
-------------------------------------------------------------------------------

instance ToJSON2 (,) where
    liftToJSON2 _ toA _ _ toB _ (a, b) = Array $ V.create $ do
        mv <- VM.unsafeNew 2
        VM.unsafeWrite mv 0 (toA a)
        VM.unsafeWrite mv 1 (toB b)
        return mv

    liftToEncoding2 _ toA _ _ toB _ (a, b) = E.list id [toA a, toB b]

instance (ToJSON a) => ToJSON1 ((,) a) where
    liftToJSON = liftToJSON2 omitField toJSON toJSONList
    liftToEncoding = liftToEncoding2 omitField toEncoding toEncodingList

instance (ToJSON a, ToJSON b) => ToJSON (a, b) where
    toJSON = toJSON2
    toEncoding = toEncoding2
    -- omitField = omitField2

instance (ToJSON a) => ToJSON2 ((,,) a) where
    liftToJSON2 _ toB _ _ toC _ (a, b, c) = Array $ V.create $ do
        mv <- VM.unsafeNew 3
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toB b)
        VM.unsafeWrite mv 2 (toC c)
        return mv

    liftToEncoding2 _ toB _ _ toC _ (a, b, c) = E.list id
      [ toEncoding a
      , toB b
      , toC c
      ]

instance (ToJSON a, ToJSON b) => ToJSON1 ((,,) a b) where
    liftToJSON = liftToJSON2 omitField toJSON toJSONList
    liftToEncoding = liftToEncoding2 omitField toEncoding toEncodingList

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a, b, c) where
    toJSON = toJSON2
    toEncoding = toEncoding2

instance (ToJSON a, ToJSON b) => ToJSON2 ((,,,) a b) where
    liftToJSON2 _ toC _ _ toD _ (a, b, c, d) = Array $ V.create $ do
        mv <- VM.unsafeNew 4
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toC c)
        VM.unsafeWrite mv 3 (toD d)
        return mv

    liftToEncoding2 _ toC _ _ toD _ (a, b, c, d) = E.list id
      [ toEncoding a
      , toEncoding b
      , toC c
      , toD d
      ]

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON1 ((,,,) a b c) where
    liftToJSON = liftToJSON2 omitField toJSON toJSONList
    liftToEncoding = liftToEncoding2 omitField toEncoding toEncodingList

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (a, b, c, d) where
    toJSON = toJSON2
    toEncoding = toEncoding2

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON2 ((,,,,) a b c) where
    liftToJSON2 _ toD _ _ toE _ (a, b, c, d, e) = Array $ V.create $ do
        mv <- VM.unsafeNew 5
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toD d)
        VM.unsafeWrite mv 4 (toE e)
        return mv

    liftToEncoding2 _ toD _ _ toE _ (a, b, c, d, e) = E.list id
      [ toEncoding a
      , toEncoding b
      , toEncoding c
      , toD d
      , toE e
      ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON1 ((,,,,) a b c d) where
    liftToJSON = liftToJSON2 omitField toJSON toJSONList
    liftToEncoding = liftToEncoding2 omitField toEncoding toEncodingList

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e) => ToJSON (a, b, c, d, e) where
    toJSON = toJSON2
    toEncoding = toEncoding2

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON2 ((,,,,,) a b c d) where
    liftToJSON2 _ toE _ _ toF _ (a, b, c, d, e, f) = Array $ V.create $ do
        mv <- VM.unsafeNew 6
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toE e)
        VM.unsafeWrite mv 5 (toF f)
        return mv

    liftToEncoding2 _ toE _ _ toF _ (a, b, c, d, e, f) = E.list id
      [ toEncoding a
      , toEncoding b
      , toEncoding c
      , toEncoding d
      , toE e
      , toF f
      ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e) => ToJSON1 ((,,,,,) a b c d e) where
    liftToJSON = liftToJSON2 omitField toJSON toJSONList
    liftToEncoding = liftToEncoding2 omitField toEncoding toEncodingList

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f) => ToJSON (a, b, c, d, e, f) where
    toJSON = toJSON2
    toEncoding = toEncoding2

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e) => ToJSON2 ((,,,,,,) a b c d e) where
    liftToJSON2 _ toF _ _ toG _ (a, b, c, d, e, f, g) = Array $ V.create $ do
        mv <- VM.unsafeNew 7
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toF f)
        VM.unsafeWrite mv 6 (toG g)
        return mv

    liftToEncoding2 _ toF _ _ toG _ (a, b, c, d, e, f, g) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toF f
        , toG g
        ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f) => ToJSON1 ((,,,,,,) a b c d e f) where
    liftToJSON = liftToJSON2 omitField toJSON toJSONList
    liftToEncoding = liftToEncoding2 omitField toEncoding toEncodingList

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g) => ToJSON (a, b, c, d, e, f, g) where
    toJSON = toJSON2
    toEncoding = toEncoding2

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f) => ToJSON2 ((,,,,,,,) a b c d e f) where
    liftToJSON2 _ toG _ _ toH _ (a, b, c, d, e, f, g, h) = Array $ V.create $ do
        mv <- VM.unsafeNew 8
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toG g)
        VM.unsafeWrite mv 7 (toH h)
        return mv

    liftToEncoding2 _ toG _ _ toH _ (a, b, c, d, e, f, g, h) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toEncoding f
        , toG g
        , toH h
        ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g) => ToJSON1 ((,,,,,,,) a b c d e f g) where
    liftToJSON = liftToJSON2 omitField toJSON toJSONList
    liftToEncoding = liftToEncoding2 omitField toEncoding toEncodingList

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h) => ToJSON (a, b, c, d, e, f, g, h) where
    toJSON = toJSON2
    toEncoding = toEncoding2

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g) => ToJSON2 ((,,,,,,,,) a b c d e f g) where
    liftToJSON2 _ toH _ _ toI _ (a, b, c, d, e, f, g, h, i) = Array $ V.create $ do
        mv <- VM.unsafeNew 9
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toJSON g)
        VM.unsafeWrite mv 7 (toH h)
        VM.unsafeWrite mv 8 (toI i)
        return mv

    liftToEncoding2 _ toH _ _ toI _ (a, b, c, d, e, f, g, h, i) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toEncoding f
        , toEncoding g
        , toH h
        , toI i
        ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h) => ToJSON1 ((,,,,,,,,) a b c d e f g h) where
    liftToJSON = liftToJSON2 omitField toJSON toJSONList
    liftToEncoding = liftToEncoding2 omitField toEncoding toEncodingList

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i) => ToJSON (a, b, c, d, e, f, g, h, i) where
    toJSON = toJSON2
    toEncoding = toEncoding2

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h) => ToJSON2 ((,,,,,,,,,) a b c d e f g h) where
    liftToJSON2 _ toI _ _ toJ _ (a, b, c, d, e, f, g, h, i, j) = Array $ V.create $ do
        mv <- VM.unsafeNew 10
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toJSON g)
        VM.unsafeWrite mv 7 (toJSON h)
        VM.unsafeWrite mv 8 (toI i)
        VM.unsafeWrite mv 9 (toJ j)
        return mv

    liftToEncoding2 _ toI _ _ toJ _ (a, b, c, d, e, f, g, h, i, j) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toEncoding f
        , toEncoding g
        , toEncoding h
        , toI i
        , toJ j
        ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i) => ToJSON1 ((,,,,,,,,,) a b c d e f g h i) where
    liftToJSON = liftToJSON2 omitField toJSON toJSONList
    liftToEncoding = liftToEncoding2 omitField toEncoding toEncodingList

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j) => ToJSON (a, b, c, d, e, f, g, h, i, j) where
    toJSON = toJSON2
    toEncoding = toEncoding2

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i) => ToJSON2 ((,,,,,,,,,,) a b c d e f g h i) where
    liftToJSON2 _ toJ _ _ toK _ (a, b, c, d, e, f, g, h, i, j, k) = Array $ V.create $ do
        mv <- VM.unsafeNew 11
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toJSON g)
        VM.unsafeWrite mv 7 (toJSON h)
        VM.unsafeWrite mv 8 (toJSON i)
        VM.unsafeWrite mv 9 (toJ j)
        VM.unsafeWrite mv 10 (toK k)
        return mv

    liftToEncoding2 _ toJ _ _ toK _ (a, b, c, d, e, f, g, h, i, j, k) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toEncoding f
        , toEncoding g
        , toEncoding h
        , toEncoding i
        , toJ j
        , toK k
        ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j) => ToJSON1 ((,,,,,,,,,,) a b c d e f g h i j) where
    liftToJSON = liftToJSON2 omitField toJSON toJSONList
    liftToEncoding = liftToEncoding2 omitField toEncoding toEncodingList

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k) => ToJSON (a, b, c, d, e, f, g, h, i, j, k) where
    toJSON = toJSON2
    toEncoding = toEncoding2

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j) => ToJSON2 ((,,,,,,,,,,,) a b c d e f g h i j) where
    liftToJSON2 _ toK _ _ toL _ (a, b, c, d, e, f, g, h, i, j, k, l) = Array $ V.create $ do
        mv <- VM.unsafeNew 12
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toJSON g)
        VM.unsafeWrite mv 7 (toJSON h)
        VM.unsafeWrite mv 8 (toJSON i)
        VM.unsafeWrite mv 9 (toJSON j)
        VM.unsafeWrite mv 10 (toK k)
        VM.unsafeWrite mv 11 (toL l)
        return mv

    liftToEncoding2 _ toK _ _ toL _ (a, b, c, d, e, f, g, h, i, j, k, l) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toEncoding f
        , toEncoding g
        , toEncoding h
        , toEncoding i
        , toEncoding j
        , toK k
        , toL l
        ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k) => ToJSON1 ((,,,,,,,,,,,) a b c d e f g h i j k) where
    liftToJSON = liftToJSON2 omitField toJSON toJSONList
    liftToEncoding = liftToEncoding2 omitField toEncoding toEncodingList

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l) => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l) where
    toJSON = toJSON2
    toEncoding = toEncoding2

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k) => ToJSON2 ((,,,,,,,,,,,,) a b c d e f g h i j k) where
    liftToJSON2 _ toL _ _ toM _ (a, b, c, d, e, f, g, h, i, j, k, l, m) = Array $ V.create $ do
        mv <- VM.unsafeNew 13
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toJSON g)
        VM.unsafeWrite mv 7 (toJSON h)
        VM.unsafeWrite mv 8 (toJSON i)
        VM.unsafeWrite mv 9 (toJSON j)
        VM.unsafeWrite mv 10 (toJSON k)
        VM.unsafeWrite mv 11 (toL l)
        VM.unsafeWrite mv 12 (toM m)
        return mv

    liftToEncoding2 _ toL _ _ toM _ (a, b, c, d, e, f, g, h, i, j, k, l, m) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toEncoding f
        , toEncoding g
        , toEncoding h
        , toEncoding i
        , toEncoding j
        , toEncoding k
        , toL l
        , toM m
        ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l) => ToJSON1 ((,,,,,,,,,,,,) a b c d e f g h i j k l) where
    liftToJSON = liftToJSON2 omitField toJSON toJSONList
    liftToEncoding = liftToEncoding2 omitField toEncoding toEncodingList

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m) => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    toJSON = toJSON2
    toEncoding = toEncoding2

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l) => ToJSON2 ((,,,,,,,,,,,,,) a b c d e f g h i j k l) where
    liftToJSON2 _ toM _ _ toN _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = Array $ V.create $ do
        mv <- VM.unsafeNew 14
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toJSON g)
        VM.unsafeWrite mv 7 (toJSON h)
        VM.unsafeWrite mv 8 (toJSON i)
        VM.unsafeWrite mv 9 (toJSON j)
        VM.unsafeWrite mv 10 (toJSON k)
        VM.unsafeWrite mv 11 (toJSON l)
        VM.unsafeWrite mv 12 (toM m)
        VM.unsafeWrite mv 13 (toN n)
        return mv

    liftToEncoding2 _ toM _ _ toN _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toEncoding f
        , toEncoding g
        , toEncoding h
        , toEncoding i
        , toEncoding j
        , toEncoding k
        , toEncoding l
        , toM m
        , toN n
        ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m) => ToJSON1 ((,,,,,,,,,,,,,) a b c d e f g h i j k l m) where
    liftToJSON = liftToJSON2 omitField toJSON toJSONList
    liftToEncoding = liftToEncoding2 omitField toEncoding toEncodingList

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m, ToJSON n) => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    toJSON = toJSON2
    toEncoding = toEncoding2

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m) => ToJSON2 ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m) where
    liftToJSON2 _ toN _ _ toO _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = Array $ V.create $ do
        mv <- VM.unsafeNew 15
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toJSON g)
        VM.unsafeWrite mv 7 (toJSON h)
        VM.unsafeWrite mv 8 (toJSON i)
        VM.unsafeWrite mv 9 (toJSON j)
        VM.unsafeWrite mv 10 (toJSON k)
        VM.unsafeWrite mv 11 (toJSON l)
        VM.unsafeWrite mv 12 (toJSON m)
        VM.unsafeWrite mv 13 (toN n)
        VM.unsafeWrite mv 14 (toO o)
        return mv

    liftToEncoding2 _ toN _ _ toO _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = E.list id
        [ toEncoding a
        , toEncoding b
        , toEncoding c
        , toEncoding d
        , toEncoding e
        , toEncoding f
        , toEncoding g
        , toEncoding h
        , toEncoding i
        , toEncoding j
        , toEncoding k
        , toEncoding l
        , toEncoding m
        , toN n
        , toO o
        ]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m, ToJSON n) => ToJSON1 ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m n) where
    liftToJSON = liftToJSON2 omitField toJSON toJSONList
    liftToEncoding = liftToEncoding2 omitField toEncoding toEncodingList

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m, ToJSON n, ToJSON o) => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    toJSON = toJSON2
    toEncoding = toEncoding2

--------------------------------------------------------------------------------

-- | Wrap a list of pairs as an object.
class Monoid pairs => FromPairs enc pairs | enc -> pairs where
  fromPairs :: pairs -> enc

instance (a ~ Value) => FromPairs (Encoding' a) Series where
  fromPairs = E.pairs
  {-# INLINE fromPairs #-}

instance FromPairs Value (DList Pair) where
  fromPairs = object . toList
  {-# INLINE fromPairs #-}

-- | Like 'KeyValue' but the value is already converted to JSON
-- ('Value' or 'Encoding'), and the result actually represents lists of pairs
-- so it can be readily concatenated.
class Monoid kv => KeyValuePair v kv where
    pair :: Key -> v -> kv

instance (v ~ Value) => KeyValuePair v (DList Pair) where
    pair k v = DList.singleton (k .= v)
    {-# INLINE pair #-}

instance (e ~ Encoding) => KeyValuePair e Series where
    pair = E.pair
    {-# INLINE pair #-}
