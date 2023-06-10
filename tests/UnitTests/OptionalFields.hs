{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module UnitTests.OptionalFields (optionalFields) where

import GHC.Generics (Generic)
import Data.Maybe (isNothing)
import UnitTests.OptionalFields.Common
import UnitTests.OptionalFields.Generics (omitGenerics)
import UnitTests.OptionalFields.TH (omitTH)
import UnitTests.OptionalFields.Manual (omitManual)

optionalFields :: TestTree
optionalFields = testGroup "optional fields"
  [ omitGenerics
  , omitTH
  , omitManual
  , proofOfConcept
  ]

-- c.f. https://github.com/haskell/aeson/pull/839#issuecomment-782453060
data P = P
  { x :: Nullable Int -- Field is required, but can be null.
  , y :: Undefineable Int -- Field is optional, but cannot be null.
  , z :: NullOrUndefineable Int -- Field is optional, and can be null.
  }
  deriving (Eq, Show, Generic)

instance ToJSON P where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

instance FromJSON P where
  parseJSON = genericParseJSON opts

newtype Nullable a = Nullable (Maybe a)
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Nullable a) where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

instance FromJSON a => FromJSON (Nullable a) where
  parseJSON = genericParseJSON opts

newtype Undefineable a = Undefineable (Maybe a)
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Undefineable a) where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts
  omitField (Undefineable a) = isNothing a

instance FromJSON a => FromJSON (Undefineable a) where
  parseJSON Null = fail "Undefineable.parseJSON: expected non-null value"
  parseJSON v = genericParseJSON opts v
  omittedField = Just (Undefineable Nothing)

newtype NullOrUndefineable a = NullOrUndefineable (Maybe a)
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (NullOrUndefineable a) where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts
  omitField (NullOrUndefineable a) = isNothing a

instance FromJSON a => FromJSON (NullOrUndefineable a) where
  parseJSON = genericParseJSON opts
  omittedField = Just (NullOrUndefineable Nothing)

opts :: Options
opts = defaultOptions { omitNothingFields = True }

fullP :: P
fullP = P (Nullable $ Just 0) (Undefineable $ Just 0) (NullOrUndefineable $ Just 0)

zero :: Key -> (Key, Value)
zero k = k .= (0 :: Int)

proofOfConcept :: TestTree
proofOfConcept = testGroup "Type-directed optional fields Proof of Concept"
  [ testGroup "toJSON"
    [ testCase "x is not omitted when Nothing" $
        let subject = fullP {x = Nullable Nothing}
            expected = object ["x" .= Null, zero "y", zero "z"]
         in toJSON subject @?= expected

    , testCase "y is omitted when Nothing" $
        let subject = fullP {y = Undefineable Nothing}
            expected = object [zero "x", zero "z"]
         in toJSON subject @?= expected

    , testCase "z is omitted when Nothing" $
        let subject = fullP {z = NullOrUndefineable Nothing}
            expected = object [zero "x", zero "y"]
         in toJSON subject @?= expected
    ]

  , testGroup "parseJSON"
    [ testCase "x can be null" $
        let subject = object ["x" .= Null, zero "y", zero "z"]
            expected = Just fullP {x = Nullable Nothing}
         in decode (encode subject) @?= expected

    , testCase "x cannot be omitted" $
        let subject = object [zero "y", zero "z"]
            expected = Nothing :: Maybe P
         in decode (encode subject) @?= expected

    , testCase "y can be omitted" $
        let subject = object [zero "x", zero "z"]
            expected = Just fullP {y = Undefineable Nothing}
         in decode (encode subject) @?= expected

    , testCase "y cannot be null" $
        let subject = object [zero "x", "y" .= Null, zero "z"]
            expected = Nothing :: Maybe P
         in decode (encode subject) @?= expected

    , testCase "z can be null" $
        let subject = object [zero "x", zero "y", "z" .= Null]
            expected = Just fullP {z = NullOrUndefineable Nothing}
         in decode (encode subject) @?= expected

    , testCase "z can be omitted" $
        let subject = object [zero "x", zero "y"]
            expected = Just fullP {z = NullOrUndefineable Nothing}
         in decode (encode subject) @?= expected
    ]
  ]
