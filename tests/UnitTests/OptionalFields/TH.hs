{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module UnitTests.OptionalFields.TH (omitTH) where

import UnitTests.OptionalFields.Common

$(deriveJSON  omittingOptions    ''RecordA)
$(deriveJSON  nonOmittingOptions ''RecordB)
$(deriveJSON  defaultOptions     ''RecordC)
$(deriveJSON1 omittingOptions    ''HRecordA)
$(deriveJSON1 nonOmittingOptions ''HRecordB)
$(deriveJSON1 defaultOptions     ''HRecordC)


instance ToJSON a => ToJSON (HRecordA a) where
  toJSON = toJSON1
  toEncoding = toEncoding1

instance FromJSON a => FromJSON (HRecordA a) where
    parseJSON = parseJSON1

instance ToJSON a => ToJSON (HRecordB a) where
  toJSON = toJSON1
  toEncoding = toEncoding1

instance FromJSON a => FromJSON (HRecordB a) where
    parseJSON = parseJSON1

instance ToJSON a => ToJSON (HRecordC a) where
  toJSON = toJSON1
  toEncoding = toEncoding1

instance FromJSON a => FromJSON (HRecordC a) where
    parseJSON = parseJSON1



omitTH :: TestTree
omitTH = testGroup "Omit optional fields (TH)"
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
