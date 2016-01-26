module DataFamilies.Properties (tests) where

import DataFamilies.Encoders
import DataFamilies.Instances ()

import Properties hiding (tests)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

--------------------------------------------------------------------------------

tests :: Test
tests = testGroup "data families" [
  testGroup "template-haskell" [
      testGroup "toJSON" [
        testGroup "Nullary" [
            testProperty "string" (isString . thNullaryToJSONString)
          , testProperty "2ElemArray" (is2ElemArray . thNullaryToJSON2ElemArray)
          , testProperty "TaggedObject" (isTaggedObjectValue . thNullaryToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thNullaryToJSONObjectWithSingleField)

          , testGroup "roundTrip" [
              testProperty "string" (toParseJSON thNullaryParseJSONString thNullaryToJSONString)
            , testProperty "2ElemArray" (toParseJSON thNullaryParseJSON2ElemArray thNullaryToJSON2ElemArray)
            , testProperty "TaggedObject" (toParseJSON thNullaryParseJSONTaggedObject thNullaryToJSONTaggedObject)
            , testProperty "ObjectWithSingleField" (toParseJSON thNullaryParseJSONObjectWithSingleField thNullaryToJSONObjectWithSingleField)
            ]
        ]
      , testGroup "SomeType" [
          testProperty "2ElemArray" (is2ElemArray . thSomeTypeToJSON2ElemArray)
        , testProperty "TaggedObject" (isTaggedObject . thSomeTypeToJSONTaggedObject)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thSomeTypeToJSONObjectWithSingleField)
        , testGroup "roundTrip" [
            testProperty "2ElemArray" (toParseJSON thSomeTypeParseJSON2ElemArray thSomeTypeToJSON2ElemArray)
          , testProperty "TaggedObject" (toParseJSON thSomeTypeParseJSONTaggedObject thSomeTypeToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (toParseJSON thSomeTypeParseJSONObjectWithSingleField thSomeTypeToJSONObjectWithSingleField)
          ]
      , testGroup "Approx" [
           testProperty "string"                (isString                . thApproxToJSONUnwrap)
         , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thApproxToJSONDefault)
         , testGroup "roundTrip" [
               testProperty "string"                (toParseJSON thApproxParseJSONUnwrap  thApproxToJSONUnwrap)
             , testProperty "ObjectWithSingleField" (toParseJSON thApproxParseJSONDefault thApproxToJSONDefault)
           ]
         ]
      , testGroup "GADT" [
          testProperty "string"                (isString                . thGADTToJSONUnwrap)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thGADTToJSONDefault)
        , testGroup "roundTrip" [
              testProperty "string"                (toParseJSON thGADTParseJSONUnwrap  thGADTToJSONUnwrap)
            , testProperty "ObjectWithSingleField" (toParseJSON thGADTParseJSONDefault thGADTToJSONDefault)
            ]
          ]
        ]
      ]
    , testGroup "toEncoding" [
        testProperty "NullaryString" $
        thNullaryToJSONString `sameAs` thNullaryToEncodingString
      , testProperty "Nullary2ElemArray" $
        thNullaryToJSON2ElemArray `sameAs` thNullaryToEncoding2ElemArray
      , testProperty "NullaryTaggedObject" $
        thNullaryToJSONTaggedObject `sameAs` thNullaryToEncodingTaggedObject
      , testProperty "NullaryObjectWithSingleField" $
        thNullaryToJSONObjectWithSingleField `sameAs`
        thNullaryToEncodingObjectWithSingleField
      , testProperty "ApproxUnwrap" $
        thApproxToJSONUnwrap `sameAs` thApproxToEncodingUnwrap
      , testProperty "ApproxDefault" $
        thApproxToJSONDefault `sameAs` thApproxToEncodingDefault
      , testProperty "SomeType2ElemArray" $
        thSomeTypeToJSON2ElemArray `sameAs` thSomeTypeToEncoding2ElemArray
      , testProperty "SomeTypeTaggedObject" $
        thSomeTypeToJSONTaggedObject `sameAs` thSomeTypeToEncodingTaggedObject
      , testProperty "SomeTypeObjectWithSingleField" $
        thSomeTypeToJSONObjectWithSingleField `sameAs`
        thSomeTypeToEncodingObjectWithSingleField
      ]
    ]
  ]
