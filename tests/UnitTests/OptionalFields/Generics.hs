{-# OPTIONS_GHC -Wno-orphans #-}

module UnitTests.OptionalFields.Generics (omitGenerics) where

import UnitTests.OptionalFields.Common

instance ToJSON RecordA where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance ToJSON RecordB where
  toJSON = genericToJSON defaultOptions { omitNothingFields = False }

omitGenerics :: TestTree
omitGenerics = testGroup "Omit optional fields (Generics)"
  [ testGroup "omitNothingFields = True"
    [ testCase "JSON should include non-optional value." $ encodeCase helloWorldRecA helloWorldObj
    , testCase "JSON should not include optional value." $ encodeCase helloRecA helloObj
    ]
  , testGroup "omitNothingFields = False"
    [ testCase "JSON should include non-optional value." $ encodeCase helloWorldRecB helloWorldObj
    , testCase "JSON should include optional value." $ encodeCase helloRecB helloNullObj
    ]
  ]
