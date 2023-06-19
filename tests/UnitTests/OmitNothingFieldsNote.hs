{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module UnitTests.OmitNothingFieldsNote (omitNothingFieldsNoteTests) where

-- prior aeson-2.2 the 'omitNothingFields' had the following note,
-- which is no longer true as these tests illustrate.

-- Setting 'omitNothingFields' to 'True' only affects fields which are of
-- type 'Maybe' /uniformly/ in the 'ToJSON' instance.
-- In particular, if the type of a field is declared as a type variable, it
-- will not be omitted from the JSON object, unless the field is
-- specialized upfront in the instance.
--
-- The same holds for 'Maybe' fields being optional in the 'FromJSON' instance.
--
-- ==== __Example__
--
-- The generic instance for the following type @Fruit@ depends on whether
-- the instance head is @Fruit a@ or @Fruit (Maybe a)@.
--
-- @
-- data Fruit a = Fruit
--   { apples :: a  -- A field whose type is a type variable.
--   , oranges :: 'Maybe' Int
--   } deriving 'Generic'
--
-- -- apples required, oranges optional
-- -- Even if 'Data.Aeson.fromJSON' is then specialized to (Fruit ('Maybe' a)).
-- instance 'Data.Aeson.FromJSON' a => 'Data.Aeson.FromJSON' (Fruit a)
--
-- -- apples optional, oranges optional
-- -- In this instance, the field apples is uniformly of type ('Maybe' a).
-- instance 'Data.Aeson.FromJSON' a => 'Data.Aeson.FromJSON' (Fruit ('Maybe' a))
--
-- options :: 'Options'
-- options = 'defaultOptions' { 'omitNothingFields' = 'True' }
--
-- -- apples always present in the output, oranges is omitted if 'Nothing'
-- instance 'Data.Aeson.ToJSON' a => 'Data.Aeson.ToJSON' (Fruit a) where
--   'Data.Aeson.toJSON' = 'Data.Aeson.genericToJSON' options
--
-- -- both apples and oranges are omitted if 'Nothing'
-- instance 'Data.Aeson.ToJSON' a => 'Data.Aeson.ToJSON' (Fruit ('Maybe' a)) where
--   'Data.Aeson.toJSON' = 'Data.Aeson.genericToJSON' options

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import GHC.Generics (Generic)
import Data.Aeson

omitNothingFieldsNoteTests :: TestTree
omitNothingFieldsNoteTests = testCase "omitNothingFields Note" $ do
    -- both fields are omitted, not only oranges!
    encode (Fruit (Nothing :: Maybe Int) Nothing) @?= "{}"

data Fruit a = Fruit
    { apples :: a  -- A field whose type is a type variable.
    , oranges :: Maybe Int
    } deriving Generic

instance ToJSON a => ToJSON (Fruit a) where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }
    toEncoding = genericToEncoding defaultOptions { omitNothingFields = True }
