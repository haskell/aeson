-- |
-- Module:      Data.Aeson
-- Copyright:   (c) 2011 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working efficiently with JSON data.
--
-- (A note on naming: in Greek mythology, Aeson was the father of Jason.)
--

module Data.Aeson
    (
    -- * Usage example
    -- $usage-example

    -- * Working directly with the JSON AST
    -- $json-ast

    -- * Pitfalls
    -- $pitfalls

    -- * Encoding and decoding
    -- $encoding-and-decoding
      decode
    , decode'
    , eitherDecode
    , eitherDecode'
    , encode
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
    -- * Inspecting @'Value's@
    , withObject
    , withText
    , withArray
    , withNumber
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
import Data.Aeson.Parser.Internal (decodeWith, eitherDecodeWith, json, json')
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as L

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- This function parses immediately, but defers conversion.  See
-- 'json' for details.
decode :: (FromJSON a) => L.ByteString -> Maybe a
decode = decodeWith json fromJSON
{-# INLINE decode #-}

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- This function parses and performs conversion immediately.  See
-- 'json'' for details.
decode' :: (FromJSON a) => L.ByteString -> Maybe a
decode' = decodeWith json' fromJSON
{-# INLINE decode' #-}

-- | Like 'decode' but returns an error message when decoding fails.
eitherDecode :: (FromJSON a) => L.ByteString -> Either String a
eitherDecode = eitherDecodeWith json fromJSON
{-# INLINE eitherDecode #-}

-- | Like 'decode'' but returns an error message when decoding fails.
eitherDecode' :: (FromJSON a) => L.ByteString -> Either String a
eitherDecode' = eitherDecodeWith json' fromJSON
{-# INLINE eitherDecode' #-}

-- $usage-example
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
-- > instance FromJSON Coord where
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
-- The explicit type signature can often be omitted as the compiler
-- can deduce the type using type inference.
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
--
-- See the documentation of 'FromJSON' and 'ToJSON' for some examples
-- how you can automatically dervice instances in some circumstances.


-- $json-ast
--
-- Sometimes you want to work with JSON data directly, without first
-- converting it to a custom data type. This can be useful if you want
-- to e.g. convert JSON data to YAML data, without knowing what the
-- contents of the original JSON data was. The 'Value' type, which is
-- an instance of 'FromJSON', is used to represent an arbitrary JSON
-- AST. Example usage:
--
-- > >>> decode "{\"foo\": 123}" :: Maybe Value
-- > Just (Object (fromList [("foo",Number 123)]))
-- > >>> decode "{\"foo\": [\"abc\",\"def\"]}" :: Maybe Value
-- > Just (Object (fromList [("foo",Array (fromList [String "abc",String "def"]))]))
--
-- Once you have a 'Value' you can write recursive functions to
-- traverse it and make arbitrary transformations.

-- $pitfalls
--
-- Note that the JSON standard only allows arrays or objects of things
-- at the top-level, so calling decode on a simple type will not work:
--
-- > >>> decode "1" :: Maybe Int
-- > Nothing
-- > >>> decode "1" :: Maybe String
-- > Nothing
--
-- Likewise, for encoding to JSON you can encode anything that's an
-- instance of 'ToJSON', which does include simple types. So beware
-- that this aspect of the API is not isomorphic:
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
-- Alternatively see 'Data.Aeson.Parser.value' to parse non-toplevel
-- JSON values.

-- $encoding-and-decoding
--
-- Encoding and decoding is a two step process. To encode a value, it
-- is first converted to a generic representation, using 'ToJSON'. The
-- generic representation is then encoded as JSON data. To decode a
-- value the process is reversed and 'FromJSON' is used instead. Both
-- these steps are combined in the 'encode' and 'decode' functions.
