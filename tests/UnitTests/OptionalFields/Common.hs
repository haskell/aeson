{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module UnitTests.OptionalFields.Common
  ( module UnitTests.OptionalFields.Common
  , module Data.Aeson
  , module Data.Aeson.TH
  , module GHC.Generics
  , module Test.Tasty
  , module Test.Tasty.HUnit
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Maybe (isNothing)
import Data.Semigroup (Semigroup (..))
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T

newtype NullableNonEmptyString = NullableNonEmptyString (Maybe String)
  deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)

instance ToJSON NullableNonEmptyString where
  toJSON (NullableNonEmptyString x) = toJSON x
  omitField (NullableNonEmptyString x) = isNothing x

instance FromJSON NullableNonEmptyString where
  parseJSON Null = pure mempty
  parseJSON (String x) = pure (nne $ T.unpack x)
  parseJSON _ = fail "NullableNonEmptyString.parseJSON: expected String or Null"

  omittedField = Just mempty

nne :: String -> NullableNonEmptyString
nne str = case filter (/= ' ') str of
  "" -> NullableNonEmptyString Nothing
  _ -> NullableNonEmptyString (Just str)

obj :: [(Key, Value)] -> Value
obj = Object . KM.fromList

prop :: ToJSON a => String -> a -> (Key, Value)
prop k v = (K.fromString k, toJSON v)

data RecordA = RecordA
  { required :: String
  , optional :: NullableNonEmptyString
  }
  deriving Generic

data RecordB = RecordB
  { required :: String
  , optional :: NullableNonEmptyString
  }
  deriving Generic

encodeCase :: HasCallStack => ToJSON a => a -> Value -> IO ()
encodeCase record object' = decode @Value (encode record) @?= Just object'

decodeCase :: forall a. HasCallStack => (FromJSON a, ToJSON a) => a -> Value -> IO ()
decodeCase record object' = (fmap encode . decode @a . encode) object' @?= Just (encode record)

counterCase :: forall a proxy. HasCallStack => (FromJSON a, ToJSON a) => proxy a -> Value -> IO ()
counterCase _ object' = assertBool "decode should fail" $ (null . decode @a . encode) object'

helloWorldRecA :: RecordA
helloWorldRecA = RecordA "hello" (nne "world")

helloWorldRecB :: RecordB
helloWorldRecB = RecordB "hello" (nne "world")

helloWorldObj :: Value
helloWorldObj = obj
  [ prop "required" "hello"
  , prop "optional" "world"
  ]

helloRecA :: RecordA
helloRecA = RecordA "hello" mempty

helloRecB :: RecordB
helloRecB = RecordB "hello" mempty

helloObj :: Value
helloObj = obj
  [ prop "required" "hello"
  ]

helloNullObj :: Value
helloNullObj = obj
  [ prop "required" "hello"
  , prop "optional" Null
  ]
