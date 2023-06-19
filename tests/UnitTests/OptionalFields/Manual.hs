{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module UnitTests.OptionalFields.Manual (omitManual) where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

import UnitTests.OptionalFields.Common

-------------------------------------------------------------------------------
-- Ordinary
-------------------------------------------------------------------------------

-- lax
instance ToJSON RecordA where
  toJSON     RecordA {..} = Object $ "required" .?= required <> "optional" .?= optional <> "default_" .?= default_
  toEncoding RecordA {..} = pairs  $ "required" .?= required <> "optional" .?= optional <> "default_" .?= default_

instance FromJSON RecordA where
    parseJSON = withObject "RecordA" $ \obj -> pure RecordA
        <*> obj .:!= "required"
        <*> obj .:!= "optional"
        <*> obj .:!= "default_"

-- strict
instance ToJSON RecordB where
  toJSON     RecordB {..} = Object $ "required" .= required <> "optional" .= optional <> "default_" .= default_
  toEncoding RecordB {..} = pairs  $ "required" .= required <> "optional" .= optional <> "default_" .= default_

instance FromJSON RecordB where
    parseJSON = withObject "RecordB" $ \obj -> pure RecordB
        <*> obj .: "required"
        <*> obj .: "optional"
        <*> obj .: "default_"

-- default: encoding strict, decoding lax
instance ToJSON RecordC where
  toJSON     RecordC {..} = Object $ "required" .= required <> "optional" .= optional <> "default_" .= default_
  toEncoding RecordC {..} = pairs  $ "required" .= required <> "optional" .= optional <> "default_" .= default_

instance FromJSON RecordC where
    parseJSON = withObject "RecordC" $ \obj -> pure RecordC
        <*> obj .:!= "required"
        <*> obj .:!= "optional"
        <*> obj .:!= "default_"

-------------------------------------------------------------------------------
-- Higher
-------------------------------------------------------------------------------

instance ToJSON1 HRecordA where
  liftToJSON     o f _ HRecordA {..} = Object $ "required" .?= required <> explicitToFieldOmit o f "optional" optional <> "default_" .?= default_
  liftToEncoding o f _ HRecordA {..} = pairs  $ "required" .?= required <> explicitToFieldOmit o f "optional" optional <> "default_" .?= default_

instance ToJSON a => ToJSON (HRecordA a) where
  toJSON = toJSON1
  toEncoding = toEncoding1

instance FromJSON1 HRecordA where
  liftParseJSON o f _ = withObject "HRecordA" $ \obj -> pure HRecordA
    <*> obj .:!= "required"
    <*> explicitParseFieldOmit o f obj "optional"
    <*> obj .:!= "default_"

instance FromJSON a => FromJSON (HRecordA a) where
    parseJSON = parseJSON1


instance ToJSON1 HRecordB where
  liftToJSON     _o f _ HRecordB {..} = Object $ "required" .= required <> explicitToField f "optional" optional <> "default_" .= default_
  liftToEncoding _o f _ HRecordB {..} = pairs  $ "required" .= required <> explicitToField f "optional" optional <> "default_" .= default_

instance ToJSON a => ToJSON (HRecordB a) where
  toJSON = toJSON1
  toEncoding = toEncoding1

instance FromJSON1 HRecordB where
  liftParseJSON _o f _ = withObject "HRecordB" $ \obj -> pure HRecordB
    <*> obj .: "required"
    <*> explicitParseField f obj "optional"
    <*> obj .: "default_"

instance FromJSON a => FromJSON (HRecordB a) where
    parseJSON = parseJSON1


instance ToJSON1 HRecordC where
  liftToJSON     _o f _ HRecordC {..} = Object $ "required" .= required <> explicitToField f "optional" optional <> "default_" .= default_
  liftToEncoding _o f _ HRecordC {..} = pairs  $ "required" .= required <> explicitToField f "optional" optional <> "default_" .= default_

instance ToJSON a => ToJSON (HRecordC a) where
  toJSON = toJSON1
  toEncoding = toEncoding1

instance FromJSON1 HRecordC where
  liftParseJSON o f _ = withObject "HRecordC" $ \obj -> pure HRecordC
    <*> obj .:!= "required"
    <*> explicitParseFieldOmit o f obj "optional"
    <*> obj .:!= "default_"

instance FromJSON a => FromJSON (HRecordC a) where
    parseJSON = parseJSON1

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

omitManual :: TestTree
omitManual = testGroup "Omit optional fields (Manual)"
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
