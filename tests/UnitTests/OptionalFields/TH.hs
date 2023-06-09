{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module UnitTests.OptionalFields.TH (omitTH) where

import UnitTests.OptionalFields.Common

$(deriveToJSON defaultOptions { omitNothingFields = True } ''RecordA)

$(deriveToJSON defaultOptions { omitNothingFields = False } ''RecordB)

omitTH :: TestTree
omitTH = testGroup "Omit optional fields (TH)"
  [ testGroup "omitNothingFields = True"
    [ testCase "JSON should include non-optional value." $ encodeCase helloWorldRecA helloWorldObj
    , testCase "JSON should not include optional value." $ encodeCase helloRecA helloObj
    ]
  , testGroup "omitNothingFields = False"
    [ testCase "JSON should include non-optional value." $ encodeCase helloWorldRecB helloWorldObj
    , testCase "JSON should include optional value." $ encodeCase helloRecB helloNullObj
    ]
  ]
