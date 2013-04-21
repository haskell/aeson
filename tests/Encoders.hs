{-# Language TemplateHaskell #-}

module Encoders where

import Data.Aeson.TH
import Data.Aeson.Types
import Options
import Types

--------------------------------------------------------------------------------
-- Nullary encoders/decoders
--------------------------------------------------------------------------------

thNullaryToJSONString :: Nullary -> Value
thNullaryToJSONString = $(mkToJSON defaultOptions ''Nullary)

thNullaryParseJSONString :: Value -> Parser Nullary
thNullaryParseJSONString = $(mkParseJSON defaultOptions ''Nullary)


thNullaryToJSON2ElemArray :: Nullary -> Value
thNullaryToJSON2ElemArray = $(mkToJSON opts2ElemArray ''Nullary)

thNullaryParseJSON2ElemArray :: Value -> Parser Nullary
thNullaryParseJSON2ElemArray = $(mkParseJSON opts2ElemArray ''Nullary)


thNullaryToJSONObjectWithType :: Nullary -> Value
thNullaryToJSONObjectWithType = $(mkToJSON optsObjectWithType ''Nullary)

thNullaryParseJSONObjectWithType :: Value -> Parser Nullary
thNullaryParseJSONObjectWithType = $(mkParseJSON optsObjectWithType ''Nullary)


thNullaryToJSONObjectWithSingleField :: Nullary -> Value
thNullaryToJSONObjectWithSingleField = $(mkToJSON optsObjectWithSingleField ''Nullary)

thNullaryParseJSONObjectWithSingleField :: Value -> Parser Nullary
thNullaryParseJSONObjectWithSingleField = $(mkParseJSON optsObjectWithSingleField ''Nullary)

gNullaryToJSONString :: Nullary -> Value
gNullaryToJSONString = genericToJSON defaultOptions

gNullaryParseJSONString :: Value -> Parser Nullary
gNullaryParseJSONString = genericParseJSON defaultOptions


gNullaryToJSON2ElemArray :: Nullary -> Value
gNullaryToJSON2ElemArray = genericToJSON opts2ElemArray

gNullaryParseJSON2ElemArray :: Value -> Parser Nullary
gNullaryParseJSON2ElemArray = genericParseJSON opts2ElemArray


gNullaryToJSONObjectWithType :: Nullary -> Value
gNullaryToJSONObjectWithType = genericToJSON optsObjectWithType

gNullaryParseJSONObjectWithType :: Value -> Parser Nullary
gNullaryParseJSONObjectWithType = genericParseJSON optsObjectWithType


gNullaryToJSONObjectWithSingleField :: Nullary -> Value
gNullaryToJSONObjectWithSingleField = genericToJSON optsObjectWithSingleField

gNullaryParseJSONObjectWithSingleField :: Value -> Parser Nullary
gNullaryParseJSONObjectWithSingleField = genericParseJSON optsObjectWithSingleField


--------------------------------------------------------------------------------
-- SomeType encoders/decoders
--------------------------------------------------------------------------------

type SomeTypeToJSON = SomeType Int -> Value

thSomeTypeToJSON2ElemArray :: ToJSON a => SomeType a -> Value
thSomeTypeToJSON2ElemArray = $(mkToJSON opts2ElemArray ''SomeType)

thSomeTypeParseJSON2ElemArray :: FromJSON a => Value -> Parser (SomeType a)
thSomeTypeParseJSON2ElemArray = $(mkParseJSON opts2ElemArray ''SomeType)


thSomeTypeToJSONObjectWithType :: ToJSON a => SomeType a -> Value
thSomeTypeToJSONObjectWithType = $(mkToJSON optsObjectWithType ''SomeType)

thSomeTypeParseJSONObjectWithType :: FromJSON a => Value -> Parser (SomeType a)
thSomeTypeParseJSONObjectWithType = $(mkParseJSON optsObjectWithType ''SomeType)


thSomeTypeToJSONObjectWithSingleField :: ToJSON a => SomeType a -> Value
thSomeTypeToJSONObjectWithSingleField = $(mkToJSON optsObjectWithSingleField ''SomeType)

thSomeTypeParseJSONObjectWithSingleField :: FromJSON a => Value -> Parser (SomeType a)
thSomeTypeParseJSONObjectWithSingleField = $(mkParseJSON optsObjectWithSingleField ''SomeType)


gSomeTypeToJSON2ElemArray :: ToJSON a => SomeType a -> Value
gSomeTypeToJSON2ElemArray = genericToJSON opts2ElemArray

gSomeTypeParseJSON2ElemArray :: FromJSON a => Value -> Parser (SomeType a)
gSomeTypeParseJSON2ElemArray = genericParseJSON opts2ElemArray


gSomeTypeToJSONObjectWithType :: ToJSON a => SomeType a -> Value
gSomeTypeToJSONObjectWithType = genericToJSON optsObjectWithType

gSomeTypeParseJSONObjectWithType :: FromJSON a => Value -> Parser (SomeType a)
gSomeTypeParseJSONObjectWithType = genericParseJSON optsObjectWithType


gSomeTypeToJSONObjectWithSingleField :: ToJSON a => SomeType a -> Value
gSomeTypeToJSONObjectWithSingleField = genericToJSON optsObjectWithSingleField

gSomeTypeParseJSONObjectWithSingleField :: FromJSON a => Value -> Parser (SomeType a)
gSomeTypeParseJSONObjectWithSingleField = genericParseJSON optsObjectWithSingleField
