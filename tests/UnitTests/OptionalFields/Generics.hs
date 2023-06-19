{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module UnitTests.OptionalFields.Generics (omitGenerics) where

import UnitTests.OptionalFields.Common

-------------------------------------------------------------------------------
-- Ordinary
-------------------------------------------------------------------------------

instance ToJSON RecordA where
  toJSON = genericToJSON omittingOptions
  toEncoding = genericToEncoding omittingOptions

instance FromJSON RecordA where
  parseJSON = genericParseJSON omittingOptions

instance ToJSON RecordB where
  toJSON = genericToJSON nonOmittingOptions
  toEncoding = genericToEncoding nonOmittingOptions

instance FromJSON RecordB where
  parseJSON = genericParseJSON nonOmittingOptions

instance ToJSON RecordC where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON RecordC where
  parseJSON = genericParseJSON defaultOptions

-------------------------------------------------------------------------------
-- Higher
-------------------------------------------------------------------------------

instance ToJSON1 HRecordA where
  liftToJSON = genericLiftToJSON omittingOptions
  liftToEncoding = genericLiftToEncoding omittingOptions

instance FromJSON1 HRecordA where
  liftParseJSON = genericLiftParseJSON omittingOptions

instance ToJSON a => ToJSON (HRecordA a) where
  toJSON = toJSON1
  toEncoding = toEncoding1

instance FromJSON a => FromJSON (HRecordA a) where
    parseJSON = parseJSON1


instance ToJSON1 HRecordB where
  liftToJSON = genericLiftToJSON nonOmittingOptions
  liftToEncoding = genericLiftToEncoding nonOmittingOptions

instance FromJSON1 HRecordB where
  liftParseJSON = genericLiftParseJSON nonOmittingOptions

instance ToJSON a => ToJSON (HRecordB a) where
  toJSON = toJSON1
  toEncoding = toEncoding1

instance FromJSON a => FromJSON (HRecordB a) where
    parseJSON = parseJSON1


instance ToJSON1 HRecordC where
  liftToJSON = genericLiftToJSON defaultOptions
  liftToEncoding = genericLiftToEncoding defaultOptions

instance FromJSON1 HRecordC where
  liftParseJSON = genericLiftParseJSON defaultOptions

instance ToJSON a => ToJSON (HRecordC a) where
  toJSON = toJSON1
  toEncoding = toEncoding1

instance FromJSON a => FromJSON (HRecordC a) where
    parseJSON = parseJSON1

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

omitGenerics :: TestTree
omitGenerics = testGroup "Omit optional fields (Generics)"
  [ testGroup "ordinary"
    [ testGroup "omitNothingFields = True"
      [ testCase "JSON should include non-optional value." $ encodeCase helloWorldRecA helloWorldObj
      , testCase "JSON should not include optional value." $ encodeCase helloRecA helloObj
      , testCase "JSON decode including non-optional value" $ decodeCase helloWorldRecA helloWorldObj
      , testCase "JSON decode not including optional value" $ decodeCase helloRecA helloObj
      , testCase "JSON decode including optional value" $ decodeCase helloRecA helloNullObj
      , testCase "JSON decode including optional value 2" $ counterCase (Proxy @RecordA) helloNullObj2
      ]
    , testGroup "omitNothingFields = False"
      [ testCase "JSON should include non-optional value." $ encodeCase helloWorldRecB helloWorldObj
      , testCase "JSON should include optional value." $ encodeCase helloRecB helloNullObj
      , testCase "JSON decode including non-optional value" $ decodeCase helloWorldRecB helloWorldObj
      , testCase "JSON decode not including optional value" $ counterCase (Proxy @RecordB) helloObj
      , testCase "JSON decode including optional value" $ decodeCase helloRecB helloNullObj
      , testCase "JSON decode including optional value 2" $ counterCase (Proxy @RecordB) helloNullObj2 -- fails because Default instance expects only numbers
      ]
    , testGroup "defaultOptions"
      [ testCase "JSON should include non-optional value." $ encodeCase helloWorldRecC helloWorldObj
      , testCase "JSON should include optional value." $ encodeCase helloRecC helloNullObj
      , testCase "JSON decode including non-optional value" $ decodeCase helloWorldRecC helloWorldObj
      , testCase "JSON decode not including optional value" $ decodeCase helloRecC helloObj
      , testCase "JSON decode including optional value" $ decodeCase helloRecC helloNullObj
      , testCase "JSON decode including optional value 2" $ counterCase (Proxy @RecordC) helloNullObj2
      ]
    ]
  , testGroup "higher"
    [ testGroup "omitNothingFields = True, higher"
      [ testCase "JSON should include non-optional value." $ encodeCase helloWorldHRecA helloWorldObj
      , testCase "JSON should not include optional value." $ encodeCase helloHRecA helloObj
      , testCase "JSON decode including non-optional value" $ decodeCase helloWorldHRecA helloWorldObj
      , testCase "JSON decode not including optional value" $ decodeCase helloHRecA helloObj
      , testCase "JSON decode including optional value" $ decodeCase helloHRecA helloNullObj
      , testCase "JSON decode including optional value 2" $ counterCase (Proxy @HRecordA') helloNullObj2
      ]
    , testGroup "omitNothingFields = False, higher"
      [ testCase "JSON should include non-optional value." $ encodeCase helloWorldHRecB helloWorldObj
      , testCase "JSON should include optional value." $ encodeCase helloHRecB helloNullObj
      , testCase "JSON decode including non-optional value" $ decodeCase helloWorldHRecB helloWorldObj
      , testCase "JSON decode not including optional value" $ counterCase (Proxy @HRecordB') helloObj
      , testCase "JSON decode including optional value" $ decodeCase helloHRecB helloNullObj
      , testCase "JSON decode including optional value 2" $ counterCase (Proxy @HRecordB') helloNullObj2
      ]
    , testGroup "defaultOptions, higher"
      [ testCase "JSON should include non-optional value." $ encodeCase helloWorldHRecC helloWorldObj
      , testCase "JSON should include optional value." $ encodeCase helloHRecC helloNullObj
      , testCase "JSON decode including non-optional value" $ decodeCase helloWorldHRecC helloWorldObj
      , testCase "JSON decode not including optional value" $ decodeCase helloHRecC helloObj
      , testCase "JSON decode including optional value" $ decodeCase helloHRecC helloNullObj
      , testCase "JSON decode including optional value 2" $ counterCase (Proxy @HRecordC') helloNullObj2
      ]
    ]
  ]
