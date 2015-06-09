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


thNullaryToJSONTaggedObject :: Nullary -> Value
thNullaryToJSONTaggedObject = $(mkToJSON optsTaggedObject ''Nullary)

thNullaryParseJSONTaggedObject :: Value -> Parser Nullary
thNullaryParseJSONTaggedObject = $(mkParseJSON optsTaggedObject ''Nullary)


thNullaryToJSONObjectWithSingleField :: Nullary -> Value
thNullaryToJSONObjectWithSingleField = $(mkToJSON optsObjectWithSingleField ''Nullary)

thNullaryParseJSONObjectWithSingleField :: Value -> Parser Nullary
thNullaryParseJSONObjectWithSingleField = $(mkParseJSON optsObjectWithSingleField ''Nullary)

thNullaryToJSONOmitEmptyContents :: Nullary -> Value
thNullaryToJSONOmitEmptyContents = $(mkToJSON optsOmitEmptyContents ''Nullary)

thNullaryParseJSONOmitEmptyContents :: Value -> Parser Nullary
thNullaryParseJSONOmitEmptyContents = $(mkParseJSON optsOmitEmptyContents ''Nullary)

gNullaryToJSONString :: Nullary -> Value
gNullaryToJSONString = genericToJSON defaultOptions

gNullaryParseJSONString :: Value -> Parser Nullary
gNullaryParseJSONString = genericParseJSON defaultOptions


gNullaryToJSON2ElemArray :: Nullary -> Value
gNullaryToJSON2ElemArray = genericToJSON opts2ElemArray

gNullaryParseJSON2ElemArray :: Value -> Parser Nullary
gNullaryParseJSON2ElemArray = genericParseJSON opts2ElemArray


gNullaryToJSONTaggedObject :: Nullary -> Value
gNullaryToJSONTaggedObject = genericToJSON optsTaggedObject

gNullaryParseJSONTaggedObject :: Value -> Parser Nullary
gNullaryParseJSONTaggedObject = genericParseJSON optsTaggedObject


gNullaryToJSONObjectWithSingleField :: Nullary -> Value
gNullaryToJSONObjectWithSingleField = genericToJSON optsObjectWithSingleField

gNullaryParseJSONObjectWithSingleField :: Value -> Parser Nullary
gNullaryParseJSONObjectWithSingleField = genericParseJSON optsObjectWithSingleField

gNullaryToJSONOmitEmptyContents :: Nullary -> Value
gNullaryToJSONOmitEmptyContents = genericToJSON optsOmitEmptyContents

gNullaryParseJSONOmitEmptyContents :: Value -> Parser Nullary
gNullaryParseJSONOmitEmptyContents = genericParseJSON optsOmitEmptyContents

--------------------------------------------------------------------------------
-- OneConstructor encoders/decoders
--------------------------------------------------------------------------------

thOneConstructorToJSONOmitEmptyContents :: OneConstructor -> Value
thOneConstructorToJSONOmitEmptyContents = $(mkToJSON optsOmitEmptyContents ''OneConstructor)

thOneConstructorParseJSONOmitEmptyContents :: Value -> Parser OneConstructor
thOneConstructorParseJSONOmitEmptyContents = $(mkParseJSON optsOmitEmptyContents ''OneConstructor)

gOneConstructorToJSONOmitEmptyContents :: OneConstructor -> Value
gOneConstructorToJSONOmitEmptyContents = genericToJSON optsOmitEmptyContents

gOneConstructorParseJSONOmitEmptyContents :: Value -> Parser OneConstructor
gOneConstructorParseJSONOmitEmptyContents = genericParseJSON optsOmitEmptyContents

--------------------------------------------------------------------------------
-- SomeType encoders/decoders
--------------------------------------------------------------------------------

type SomeTypeToJSON = SomeType Int -> Value

thSomeTypeToJSON2ElemArray :: ToJSON a => SomeType a -> Value
thSomeTypeToJSON2ElemArray = $(mkToJSON opts2ElemArray ''SomeType)

thSomeTypeParseJSON2ElemArray :: FromJSON a => Value -> Parser (SomeType a)
thSomeTypeParseJSON2ElemArray = $(mkParseJSON opts2ElemArray ''SomeType)


thSomeTypeToJSONTaggedObject :: ToJSON a => SomeType a -> Value
thSomeTypeToJSONTaggedObject = $(mkToJSON optsTaggedObject ''SomeType)

thSomeTypeParseJSONTaggedObject :: FromJSON a => Value -> Parser (SomeType a)
thSomeTypeParseJSONTaggedObject = $(mkParseJSON optsTaggedObject ''SomeType)


thSomeTypeToJSONObjectWithSingleField :: ToJSON a => SomeType a -> Value
thSomeTypeToJSONObjectWithSingleField = $(mkToJSON optsObjectWithSingleField ''SomeType)

thSomeTypeParseJSONObjectWithSingleField :: FromJSON a => Value -> Parser (SomeType a)
thSomeTypeParseJSONObjectWithSingleField = $(mkParseJSON optsObjectWithSingleField ''SomeType)

thSomeTypeToJSONOmitEmptyContents :: ToJSON a => SomeType a -> Value
thSomeTypeToJSONOmitEmptyContents = $(mkToJSON optsOmitEmptyContents ''SomeType)

thSomeTypeParseJSONOmitEmptyContents :: FromJSON a => Value -> Parser (SomeType a)
thSomeTypeParseJSONOmitEmptyContents = $(mkParseJSON optsOmitEmptyContents ''SomeType)

gSomeTypeToJSON2ElemArray :: ToJSON a => SomeType a -> Value
gSomeTypeToJSON2ElemArray = genericToJSON opts2ElemArray

gSomeTypeParseJSON2ElemArray :: FromJSON a => Value -> Parser (SomeType a)
gSomeTypeParseJSON2ElemArray = genericParseJSON opts2ElemArray


gSomeTypeToJSONTaggedObject :: ToJSON a => SomeType a -> Value
gSomeTypeToJSONTaggedObject = genericToJSON optsTaggedObject

gSomeTypeParseJSONTaggedObject :: FromJSON a => Value -> Parser (SomeType a)
gSomeTypeParseJSONTaggedObject = genericParseJSON optsTaggedObject


gSomeTypeToJSONObjectWithSingleField :: ToJSON a => SomeType a -> Value
gSomeTypeToJSONObjectWithSingleField = genericToJSON optsObjectWithSingleField

gSomeTypeParseJSONObjectWithSingleField :: FromJSON a => Value -> Parser (SomeType a)
gSomeTypeParseJSONObjectWithSingleField = genericParseJSON optsObjectWithSingleField

gSomeTypeToJSONOmitEmptyContents :: ToJSON a => SomeType a -> Value
gSomeTypeToJSONOmitEmptyContents = genericToJSON optsOmitEmptyContents

gSomeTypeParseJSONOmitEmptyContents :: FromJSON a => Value -> Parser (SomeType a)
gSomeTypeParseJSONOmitEmptyContents = genericParseJSON optsOmitEmptyContents
--------------------------------------------------------------------------------
-- Approx encoders/decoders
--------------------------------------------------------------------------------

type ApproxToJSON = Approx String -> Value

thApproxToJSONUnwrap :: ToJSON a => Approx a -> Value
thApproxToJSONUnwrap = $(mkToJSON optsUnwrapUnaryRecords ''Approx)

thApproxParseJSONUnwrap :: FromJSON a => Value -> Parser (Approx a)
thApproxParseJSONUnwrap = $(mkParseJSON optsUnwrapUnaryRecords ''Approx)


thApproxToJSONDefault :: ToJSON a => Approx a -> Value
thApproxToJSONDefault = $(mkToJSON defaultOptions ''Approx)

thApproxParseJSONDefault :: FromJSON a => Value -> Parser (Approx a)
thApproxParseJSONDefault = $(mkParseJSON defaultOptions ''Approx)

gApproxToJSONUnwrap :: Approx String -> Value
gApproxToJSONUnwrap = genericToJSON optsUnwrapUnaryRecords

gApproxParseJSONUnwrap :: Value -> Parser (Approx String)
gApproxParseJSONUnwrap = genericParseJSON optsUnwrapUnaryRecords


gApproxToJSONDefault :: Approx String -> Value
gApproxToJSONDefault = genericToJSON defaultOptions

gApproxParseJSONDefault :: Value -> Parser (Approx String)
gApproxParseJSONDefault = genericParseJSON defaultOptions
