{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, RankNTypes #-}
module Data.Aeson.Encoding
    (
    -- * Encoding
      Encoding (..) -- TODO: export fromEncoding for now
    , fromEncoding
    , unsafeToEncoding
    , Series (..) -- TODO: don't export constructor
    , pairs
    -- * Encoding constructors
    , emptyArray_
    , emptyObject_
    , wrapArray
    , wrapObject
    , text
    , list
    , dict
    , tuple
    , (>*<)
    -- ** chars
    , comma, colon, openBracket, closeBracket, openCurly, closeCurly
    ) where

import Data.Text (Text)
import Data.ByteString.Builder (Builder, char7, toLazyByteString)
import Data.Semigroup (Semigroup((<>)))
import Data.ByteString.Builder.Prim (primBounded)
import Data.Typeable (Typeable)

import qualified Data.Aeson.Encode.Builder as B

-- | An encoding of a JSON value.
newtype Encoding = Encoding {
      fromEncoding :: Builder
      -- ^ Acquire the underlying bytestring builder.
    } deriving (Semigroup,Monoid,Typeable)

-- | Make Encoding from Builder.
--
-- Use with care! You have to make sure that the passed Builder
-- is a valid JSON Encoding!
unsafeToEncoding :: Builder -> Encoding
unsafeToEncoding = Encoding

-------------------------------------------------------------------------------
-- Encoding instances
-------------------------------------------------------------------------------

instance Show Encoding where
    show (Encoding e) = show (toLazyByteString e)

instance Eq Encoding where
    Encoding a == Encoding b = toLazyByteString a == toLazyByteString b

instance Ord Encoding where
    compare (Encoding a) (Encoding b) =
      compare (toLazyByteString a) (toLazyByteString b)

-- | A series of values that, when encoded, should be separated by
-- commas. Since 0.11.0.0, the '.=' operator is overloaded to create
-- either @(Text, Value)@ or 'Series'. You can use Series when
-- encoding directly to a bytestring builder as in the following
-- example:
--
-- > toEncoding (Person name age) = pairs ("name" .= name <> "age" .= age)
data Series = Empty
            | Value Encoding
            deriving (Typeable)

instance Semigroup Series where
    Empty   <> a = a
    Value a <> b =
        Value $
        a <> case b of
               Empty   -> mempty
               Value c -> Encoding (char7 ',') <> c

instance Monoid Series where
    mempty  = Empty
    mappend = (<>)

emptyArray_ :: Encoding
emptyArray_ = Encoding B.emptyArray_

emptyObject_ :: Encoding
emptyObject_ = Encoding B.emptyObject_

wrapArray :: Encoding -> Encoding
wrapArray e = openBracket <> e <> closeBracket

wrapObject :: Encoding -> Encoding
wrapObject e = openCurly <> e <> closeCurly

-- | Encode a series of key/value pairs, separated by commas.
pairs :: Series -> Encoding
pairs = brackets '{' '}'
{-# INLINE pairs #-}

brackets :: Char -> Char -> Series -> Encoding
brackets begin end (Value v) = Encoding $
                               char7 begin <> fromEncoding v <> char7 end
brackets begin end Empty     = Encoding (primBounded (B.ascii2 (begin,end)) ())

list :: (a -> Encoding) -> [a] -> Encoding
list _  []     = emptyArray_
list to' (x:xs) = Encoding $
    char7 '[' <> fromEncoding (to' x) <> commas xs <> char7 ']'
  where
    commas = foldr (\v vs -> char7 ',' <> fromEncoding (to' v) <> vs) mempty
{-# INLINE list #-}

-- | Encode as JSON object
dict
    :: (k -> Encoding)                                -- ^ key encoding
    -> (v -> Encoding)                                -- ^ value encoding
    -> (forall a. (k -> v -> a -> a) -> a -> m -> a)  -- ^ @foldrWithKey@ - indexed fold
    -> m                                              -- ^ container
    -> Encoding
dict encodeKey encodeVal foldrWithKey = brackets '{' '}' . foldrWithKey go mempty
  where
    go k v c = Value (encodeKV k v) <> c
    encodeKV k v = encodeKey k <> Encoding (char7 ':') <> encodeVal v
{-# INLINE dict #-}

infixr 6 >*<
-- | See 'tuple'.
(>*<) :: Encoding -> Encoding -> Encoding
Encoding a >*< Encoding b = Encoding $ a <> char7 ',' <> b
{-# INLINE (>*<) #-}

-- | Encode as a tuple.
--
-- @
-- toEncoding (X a b c) = tuple $
--     toEncoding a >*<
--     toEncoding b >*<
--     toEncoding c
tuple :: Encoding -> Encoding
tuple b = Encoding (char7 '[' <> fromEncoding b <> char7 ']')
{-# INLINE tuple #-}

text :: Text -> Encoding
text = Encoding . B.text

-------------------------------------------------------------------------------
-- chars
-------------------------------------------------------------------------------

comma, colon, openBracket, closeBracket, openCurly, closeCurly :: Encoding
comma        = Encoding $ char7 ','
colon        = Encoding $ char7 ':'
openBracket  = Encoding $ char7 '['
closeBracket = Encoding $ char7 ']'
openCurly    = Encoding $ char7 '{'
closeCurly   = Encoding $ char7 '}'
