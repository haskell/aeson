{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module UnitTests.OptionalFields.Common
  ( module UnitTests.OptionalFields.Common
  , module Data.Aeson
  , module Data.Aeson.Types
  , module Data.Aeson.TH
  , module Test.Tasty
  , module Test.Tasty.HUnit
  , module Data.Proxy
  ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Maybe (isNothing)
import GHC.Generics (Generic, Generic1)
import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Field types
-------------------------------------------------------------------------------

newtype NullableNonEmptyString = NullableNonEmptyString (Maybe String)
  deriving (Eq, Show)

defaultNullableNonEmptyString :: NullableNonEmptyString
defaultNullableNonEmptyString = NullableNonEmptyString Nothing

instance ToJSON NullableNonEmptyString where
  toJSON (NullableNonEmptyString x) = toJSON x
  toEncoding (NullableNonEmptyString x) = toEncoding x
  omitField (NullableNonEmptyString x) = isNothing x

instance FromJSON NullableNonEmptyString where
  parseJSON Null = pure defaultNullableNonEmptyString
  parseJSON (String x) = pure (nne $ T.unpack x)
  parseJSON _ = fail "NullableNonEmptyString.parseJSON: expected String or Null"

  omittedField = Just defaultNullableNonEmptyString



nne :: String -> NullableNonEmptyString
nne str = case filter (/= ' ') str of
  "" -> NullableNonEmptyString Nothing
  _ -> NullableNonEmptyString (Just str)

newtype Default = Default Int
  deriving (Eq, Show)

instance ToJSON Default where
    toJSON (Default i) = toJSON i
    toEncoding (Default i) = toEncoding i
    omitField (Default i) = i == 0

instance FromJSON Default where
    parseJSON = fmap Default . parseJSON
    omittedField = Just (Default 0)

-------------------------------------------------------------------------------
-- Records
-------------------------------------------------------------------------------

-- lax
data RecordA = RecordA
  { required :: String
  , optional :: NullableNonEmptyString
  , default_ :: Default
  }
  deriving (Eq, Show, Generic)

-- strict
data RecordB = RecordB
  { required :: String
  , optional :: NullableNonEmptyString
  , default_ :: Default
  }
  deriving (Eq, Show, Generic)

-- default
data RecordC = RecordC
  { required :: String
  , optional :: NullableNonEmptyString
  , default_ :: Default
  }
  deriving (Eq, Show, Generic)

data HRecordA a = HRecordA
  { required :: String
  , optional :: a
  , default_ :: Default
  }
  deriving (Eq, Show, Generic1)

data HRecordB a = HRecordB
  { required :: String
  , optional :: a
  , default_ :: Default
  }
  deriving (Eq, Show, Generic1)

data HRecordC a = HRecordC
  { required :: String
  , optional :: a
  , default_ :: Default
  }
  deriving (Eq, Show, Generic1)

type HRecordA' = HRecordA NullableNonEmptyString
type HRecordB' = HRecordB NullableNonEmptyString
type HRecordC' = HRecordC NullableNonEmptyString

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

nonOmittingOptions :: Options
nonOmittingOptions = defaultOptions { omitNothingFields = False, allowOmittedFields = False }

omittingOptions :: Options
omittingOptions = defaultOptions { omitNothingFields = True, allowOmittedFields = True }

-------------------------------------------------------------------------------
-- Test utils
-------------------------------------------------------------------------------

encodeCase :: HasCallStack => ToJSON a => a -> Value -> IO ()
encodeCase record obj = do
  decode @Value (encode record)          @?= Just obj
  decode @Value (encode (toJSON record)) @?= Just obj

decodeCase :: forall a. HasCallStack => (FromJSON a, Eq a, Show a) => a -> Value -> IO ()
decodeCase record obj = do
  decode @a (encode obj) @?= Just record

counterCase :: forall a proxy. HasCallStack => (FromJSON a, ToJSON a, Show a) => proxy a -> Value -> IO ()
counterCase _ obj = case decode @a (encode obj) of
  Nothing -> return ()
  Just v  -> assertFailure $ "decode should fail, got: " ++ show v

-------------------------------------------------------------------------------
-- Test inputs
-------------------------------------------------------------------------------

helloWorldRecA :: RecordA
helloWorldRecA = RecordA "hello" (nne "world") (Default 42)

helloWorldRecB :: RecordB
helloWorldRecB = RecordB "hello" (nne "world") (Default 42)

helloWorldRecC :: RecordC
helloWorldRecC = RecordC "hello" (nne "world") (Default 42)

helloWorldHRecA :: HRecordA NullableNonEmptyString
helloWorldHRecA = HRecordA "hello" (nne "world") (Default 42)

helloWorldHRecB :: HRecordB NullableNonEmptyString
helloWorldHRecB = HRecordB "hello" (nne "world") (Default 42)

helloWorldHRecC :: HRecordC NullableNonEmptyString
helloWorldHRecC = HRecordC "hello" (nne "world") (Default 42)

helloWorldObj :: Value
helloWorldObj = object
  [ "required" .= String "hello"
  , "optional" .= String "world"
  , "default_" .= Number 42
  ]

helloRecA :: RecordA
helloRecA = RecordA "hello" defaultNullableNonEmptyString (Default 0)

helloRecB :: RecordB
helloRecB = RecordB "hello" defaultNullableNonEmptyString (Default 0)

helloRecC :: RecordC
helloRecC = RecordC "hello" defaultNullableNonEmptyString (Default 0)

helloHRecA :: HRecordA NullableNonEmptyString
helloHRecA = HRecordA "hello" defaultNullableNonEmptyString (Default 0)

helloHRecB :: HRecordB NullableNonEmptyString
helloHRecB = HRecordB "hello" defaultNullableNonEmptyString (Default 0)

helloHRecC :: HRecordC NullableNonEmptyString
helloHRecC = HRecordC "hello" defaultNullableNonEmptyString (Default 0)

helloObj :: Value
helloObj = object
  [ "required" .= String "hello"
  ]

helloNullObj :: Value
helloNullObj = object
  [ "required" .= String "hello"
  , "optional" .= Null
  , "default_" .= Number 0
  ]

helloNullObj2 :: Value
helloNullObj2 = object
  [ "required" .= String "hello"
  , "optional" .= Null
  , "default_" .= Null
  ]
