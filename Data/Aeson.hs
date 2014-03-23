{-# LANGUAGE CPP #-}

-- |
-- Module:      Data.Aeson
-- Copyright:   (c) 2011, 2012 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working efficiently with JSON data.
--
-- (A note on naming: in Greek mythology, Aeson was the father of Jason.)

module Data.Aeson
    (
    -- * How to use this library
    -- $use

    -- ** Working with the AST
    -- $ast

    -- ** Decoding to a Haskell value
    -- $haskell

    -- ** Decoding a mixed-type object
    -- $mixed

    -- ** Automatically decoding data types
    -- $typeable

    -- ** Pitfalls
    -- $pitfalls

    -- * Encoding and decoding
    -- $encoding_and_decoding
      decode
    , decode'
    , eitherDecode
    , eitherDecode'
    , encode
    -- ** Variants for strict bytestrings
    , decodeStrict
    , decodeStrict'
    , eitherDecodeStrict
    , eitherDecodeStrict'
    -- * Core JSON types
    , Value(..)
    , Array
    , Object
    -- * Convenience types
    , DotNetTime(..)
    -- * Type conversion
    , FromJSON(..)
    , Result(..)
    , fromJSON
    , ToJSON(..)
#ifdef GENERICS
    -- ** Generic JSON classes
    , GFromJSON(..)
    , GToJSON(..)
    , genericToJSON
    , genericParseJSON
#endif
    -- * Inspecting @'Value's@
    , withObject
    , withText
    , withArray
    , withNumber
    , withScientific
    , withBool
    -- * Constructors and accessors
    , (.=)
    , (.:)
    , (.:?)
    , (.!=)
    , object
    -- * Parsing
    , json
    , json'
    ) where

import Data.Aeson.Encode (encode)
import Data.Aeson.Parser.Internal (decodeWith, decodeStrictWith,
                                   eitherDecodeWith, eitherDecodeStrictWith,
                                   jsonEOF, json, jsonEOF', json')
import Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input must consist solely of a JSON document, with no trailing
-- data except for whitespace. This restriction is necessary to ensure
-- that if data is being lazily read from a file handle, the file
-- handle will be closed in a timely fashion once the document has
-- been parsed.
--
-- This function parses immediately, but defers conversion.  See
-- 'json' for details.
decode :: (FromJSON a) => L.ByteString -> Maybe a
decode = decodeWith jsonEOF fromJSON
{-# INLINE decode #-}

-- | Efficiently deserialize a JSON value from a strict 'B.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input must consist solely of a JSON document, with no trailing
-- data except for whitespace.
--
-- This function parses immediately, but defers conversion.  See
-- 'json' for details.
decodeStrict :: (FromJSON a) => B.ByteString -> Maybe a
decodeStrict = decodeStrictWith jsonEOF fromJSON
{-# INLINE decodeStrict #-}

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input must consist solely of a JSON document, with no trailing
-- data except for whitespace. This restriction is necessary to ensure
-- that if data is being lazily read from a file handle, the file
-- handle will be closed in a timely fashion once the document has
-- been parsed.
--
-- This function parses and performs conversion immediately.  See
-- 'json'' for details.
decode' :: (FromJSON a) => L.ByteString -> Maybe a
decode' = decodeWith jsonEOF' fromJSON
{-# INLINE decode' #-}

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input must consist solely of a JSON document, with no trailing
-- data except for whitespace.
--
-- This function parses and performs conversion immediately.  See
-- 'json'' for details.
decodeStrict' :: (FromJSON a) => B.ByteString -> Maybe a
decodeStrict' = decodeStrictWith jsonEOF' fromJSON
{-# INLINE decodeStrict' #-}

-- | Like 'decode' but returns an error message when decoding fails.
eitherDecode :: (FromJSON a) => L.ByteString -> Either String a
eitherDecode = eitherDecodeWith jsonEOF fromJSON
{-# INLINE eitherDecode #-}

-- | Like 'decodeStrict' but returns an error message when decoding fails.
eitherDecodeStrict :: (FromJSON a) => B.ByteString -> Either String a
eitherDecodeStrict = eitherDecodeStrictWith jsonEOF fromJSON
{-# INLINE eitherDecodeStrict #-}

-- | Like 'decode'' but returns an error message when decoding fails.
eitherDecode' :: (FromJSON a) => L.ByteString -> Either String a
eitherDecode' = eitherDecodeWith jsonEOF' fromJSON
{-# INLINE eitherDecode' #-}

-- | Like 'decodeStrict'' but returns an error message when decoding fails.
eitherDecodeStrict' :: (FromJSON a) => B.ByteString -> Either String a
eitherDecodeStrict' = eitherDecodeStrictWith jsonEOF' fromJSON
{-# INLINE eitherDecodeStrict' #-}

-- $use
--
-- This section contains basic information on the different ways to
-- decode data using this library. These range from simple but
-- inflexible, to complex but flexible.
--
-- The most common way to use the library is to define a data type,
-- corresponding to some JSON data you want to work with, and then
-- write either a 'FromJSON' instance, a to 'ToJSON' instance, or both
-- for that type. For example, given this JSON data:
--
-- > { "name": "Joe", "age": 12 }
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
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > instance FromJSON Person where
-- >     parseJSON (Object v) = Person <$>
-- >                            v .: "name" <*>
-- >                            v .: "age"
-- >     -- A non-Object value is of the wrong type, so fail.
-- >     parseJSON _          = mzero
--
-- We can now parse the JSON data like so:
--
-- > >>> decode "{\"name\":\"Joe\",\"age\":12}" :: Maybe Person
-- > Just (Person {name = "Joe", age = 12})
--
-- To encode data, we need to define a 'ToJSON' instance:
--
-- > instance ToJSON Person where
-- >     toJSON (Person name age) = object ["name" .= name, "age" .= age]
--
-- We can now encode a value like so:
--
-- > >>> encode (Person {name = "Joe", age = 12})
-- > "{\"name\":\"Joe\",\"age\":12}"
--
-- There are predefined 'FromJSON' and 'ToJSON' instances for many
-- types. Here's an example using lists and 'Int's:
--
-- > >>> decode "[1,2,3]" :: Maybe [Int]
-- > Just [1,2,3]
--
-- And here's an example using the 'Data.Map.Map' type to get a map of
-- 'Int's.
--
-- > >>> decode "{\"foo\":1,\"bar\":2}" :: Maybe (Map String Int)
-- > Just (fromList [("bar",2),("foo",1)])

-- While the notes below focus on decoding, you can apply almost the
-- same techniques to /encoding/ data. (The main difference is that
-- encoding always succeeds, but decoding has to handle the
-- possibility of failure, where an input doesn't match our
-- expectations.)
--
-- See the documentation of 'FromJSON' and 'ToJSON' for some examples
-- of how you can automatically derive instances in some
-- circumstances.

-- $ast
--
-- Sometimes you want to work with JSON data directly, without first
-- converting it to a custom data type. This can be useful if you want
-- to e.g. convert JSON data to YAML data, without knowing what the
-- contents of the original JSON data was. The 'Value' type, which is
-- an instance of 'FromJSON', is used to represent an arbitrary JSON
-- AST (abstract syntax tree). Example usage:
--
-- > >>> decode "{\"foo\": 123}" :: Maybe Value
-- > Just (Object (fromList [("foo",Number 123)]))
--
-- > >>> decode "{\"foo\": [\"abc\",\"def\"]}" :: Maybe Value
-- > Just (Object (fromList [("foo",Array (fromList [String "abc",String "def"]))]))
--
-- Once you have a 'Value' you can write functions to traverse it and
-- make arbitrary transformations.

-- $haskell
--
-- Any instance of 'FromJSON' can be specified (but see the
-- \"Pitfalls\" section here&#8212;"Data.Aeson#pitfalls"):
--
-- > λ> decode "[1,2,3]" :: Maybe [Int]
-- > Just [1,2,3]
--
-- Alternatively, there are instances for standard data types, so you
-- can use them directly. For example, use the 'Data.Map.Map' type to
-- get a map of 'Int's.
--
-- > λ> :m + Data.Map
-- > λ> decode "{\"foo\":1,\"bar\":2}" :: Maybe (Map String Int)
-- > Just (fromList [("bar",2),("foo",1)])

-- $mixed
--
-- The above approach with maps of course will not work for mixed-type
-- objects that don't follow a strict schema, but there are a couple
-- of approaches available for these.
--
-- The 'Object' type contains JSON objects:
--
-- > λ> decode "{\"name\":\"Dave\",\"age\":2}" :: Maybe Object
-- > Just (fromList) [("name",String "Dave"),("age",Number 2)]
--
-- You can extract values from it with a parser using 'parse',
-- 'parseEither' or, in this example, 'parseMaybe':
--
-- > λ> do result <- decode "{\"name\":\"Dave\",\"age\":2}"
-- >       flip parseMaybe result $ \obj -> do
-- >         age <- obj .: "age"
-- >         name <- obj .: "name"
-- >         return (name ++ ": " ++ show (age*2))
-- >
-- > Just "Dave: 4"
--
-- Considering that any type that implements 'FromJSON' can be used
-- here, this is quite a powerful way to parse JSON. See the
-- documentation in 'FromJSON' for how to implement this class for
-- your own data types.
--
-- The downside is that you have to write the parser yourself; the
-- upside is that you have complete control over the way the JSON is
-- parsed.

-- $typeable
--
-- If you don't want fine control and would prefer the JSON be parsed
-- to your own data types automatically according to some reasonably
-- sensible isomorphic implementation, you can use the generic parser
-- based on 'Data.Typeable.Typeable' and 'Data.Data.Data'. Switch to
-- the 'Data.Aeson.Generic' module, and you can do the following:
--
-- > λ> decode "[1]" :: Maybe [Int]
-- > Just [1]
-- > λ> :m + Data.Typeable Data.Data
-- > λ> :set -XDeriveDataTypeable
-- > λ> data Person = Person { personName :: String, personAge :: Int } deriving (Data,Typeable,Show)
-- > λ> encode Person { personName = "Chris", personAge = 123 }
-- > "{\"personAge\":123,\"personName\":\"Chris\"}"
-- > λ> decode "{\"personAge\":123,\"personName\":\"Chris\"}" :: Maybe Person
-- > Just (Person {
-- > personName = "Chris", personAge = 123
-- > })
--
-- Be aware that the encoding may not always be what you'd naively
-- expect:
--
-- > λ> data Foo = Foo Int Int deriving (Data,Typeable,Show)
-- > λ> encode (Foo 1 2)
-- > "[1,2]"
--
-- With this approach, it's best to treat the
-- 'Data.Aeson.Generic.decode' and 'Data.Aeson.Generic.encode'
-- functions as an isomorphism, and not to rely upon (or care about)
-- the specific intermediate representation.

-- $pitfalls
-- #pitfalls#
--
-- Note that the JSON standard requires that the top-level value be
-- either an array or an object. If you try to use 'decode' with a
-- result type that is /not/ represented in JSON as an array or
-- object, your code will typecheck, but it will always \"fail\" at
-- runtime:
--
-- > >>> decode "1" :: Maybe Int
-- > Nothing
-- > >>> decode "1" :: Maybe String
-- > Nothing
--
-- So stick to objects (e.g. maps in Haskell) or arrays (lists or
-- vectors in Haskell):
--
-- > >>> decode "[1,2,3]" :: Maybe [Int]
-- > Just [1,2,3]
--
-- When encoding to JSON you can encode anything that's an instance of
-- 'ToJSON', and this may include simple types. So beware that this
-- aspect of the API is not isomorphic. You can round-trip arrays and
-- maps, but not simple values:
--
-- > >>> encode [1,2,3]
-- > "[1,2,3]"
-- > >>> decode (encode [1]) :: Maybe [Int]
-- > Just [1]
-- > >>> encode 1
-- > "1"
-- > >>> decode (encode (1 :: Int)) :: Maybe Int
-- > Nothing
--
-- Alternatively, see 'Data.Aeson.Parser.value' to parse non-top-level
-- JSON values.

-- $encoding_and_decoding
--
-- Encoding and decoding are each two-step processes.
--
-- * To encode a value, it is first converted to an abstract syntax
--   tree (AST), using 'ToJSON'. This generic representation is then
--   encoded as bytes.
--
-- * When decoding a value, the process is reversed: the bytes are
--   converted to an AST, then the 'FromJSON' class is used to convert
--   to the desired type.
--
-- For convenience, the 'encode' and 'decode' functions combine both
-- steps.
