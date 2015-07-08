-- Adapted from a buffer-builder benchmark:
--
-- https://github.com/chadaustin/buffer-builder/blob/master/test.json

{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash, ScopedTypeVariables #-}

module Compare.JsonBench (benchmarks) where

import Control.DeepSeq (NFData(..))
import Criterion
import Data.Aeson ((.:))
import Data.Monoid ((<>))
import Data.Text (Text)
import Typed.Common (load)
import qualified Data.Aeson as Aeson
import qualified Data.BufferBuilder.Json as Json
import qualified Data.Json.Builder as JB

data EyeColor = Green | Blue | Brown
    deriving (Eq, Show)
data Gender = Male | Female
    deriving (Eq, Show)
data Fruit = Apple | Strawberry | Banana
    deriving (Eq, Show)
data Friend = Friend
    { fId :: !Int
    , fName :: !Text
    } deriving (Eq, Show)

data User = User
    { uId       :: !Text
    , uIndex    :: !Int
    , uGuid     :: !Text
    , uIsActive :: !Bool
    , uBalance  :: !Text
    , uPicture  :: !Text
    , uAge      :: !Int
    , uEyeColor :: !EyeColor
    , uName     :: !Text
    , uGender   :: !Gender
    , uCompany  :: !Text
    , uEmail    :: !Text
    , uPhone    :: !Text
    , uAddress  :: !Text
    , uAbout    :: !Text
    , uRegistered   :: !Text -- UTCTime?
    , uLatitude :: !Double
    , uLongitude    :: !Double
    , uTags :: ![Text]
    , uFriends  :: ![Friend]
    , uGreeting :: !Text
    , uFavouriteFruit   :: !Fruit
    } deriving (Eq, Show)

instance NFData EyeColor where rnf !_ = ()
instance NFData Gender where rnf !_ = ()
instance NFData Fruit where rnf !_ = ()

instance NFData Friend where
    rnf Friend {..} = (rnf fId) `seq` (rnf fName) `seq` ()

instance NFData User where
    rnf User {..} = (rnf uId) `seq` (rnf uIndex) `seq` (rnf uGuid) `seq` (rnf uIsActive) `seq` (rnf uBalance) `seq` (rnf uPicture) `seq` (rnf uAge) `seq` (rnf uEyeColor) `seq` (rnf uName) `seq` (rnf uGender) `seq` (rnf uCompany) `seq` (rnf uEmail) `seq` (rnf uPhone) `seq` (rnf uAddress) `seq` (rnf uAbout) `seq` (rnf uRegistered) `seq` (rnf uLatitude) `seq` (rnf uLongitude) `seq` (rnf uTags) `seq` (rnf uFriends) `seq` (rnf uGreeting) `seq` (rnf uFavouriteFruit) `seq` ()

eyeColorTable :: [(Text, EyeColor)]
eyeColorTable = [("brown", Brown), ("green", Green), ("blue", Blue)]

genderTable :: [(Text, Gender)]
genderTable = [("male", Male), ("female", Female)]

fruitTable :: [(Text, Fruit)]
fruitTable = [("apple", Apple), ("strawberry", Strawberry), ("banana", Banana)]

enumFromJson :: Monad m => String -> [(Text, enum)] -> (json -> m Text) -> json -> m enum
enumFromJson enumName table extract v = do
    s <- extract v
    case lookup s table of
        Just r -> return r
        Nothing -> fail $ "Bad " ++ enumName ++ ": " ++ show s

--- Aeson instances ---

instance Aeson.FromJSON EyeColor where
    parseJSON = enumFromJson "EyeColor" eyeColorTable Aeson.parseJSON

instance Aeson.FromJSON Gender where
    parseJSON = enumFromJson "Gender" genderTable Aeson.parseJSON

instance Aeson.FromJSON Fruit where
    parseJSON = enumFromJson "Fruit" fruitTable Aeson.parseJSON

instance Aeson.FromJSON Friend where
    parseJSON = Aeson.withObject "Friend" $ \o -> do
        fId <- o .: "id"
        fName <- o .: "name"
        return Friend {..}

instance Aeson.FromJSON User where
    parseJSON = Aeson.withObject "User" $ \o -> do
        uId <- o .: "_id"
        uIndex <- o .: "index"
        uGuid <- o .: "guid"
        uIsActive <- o .: "isActive"
        uBalance <- o .: "balance"
        uPicture <- o .: "picture"
        uAge <- o .: "age"
        uEyeColor <- o .: "eyeColor"
        uName <- o .: "name"
        uGender <- o .: "gender"
        uCompany <- o .: "company"
        uEmail <- o .: "email"
        uPhone <- o .: "phone"
        uAddress <- o .: "address"
        uAbout <- o .: "about"
        uRegistered <- o .: "registered"
        uLatitude <- o .: "latitude"
        uLongitude <- o .: "longitude"
        uTags <- o .: "tags"
        uFriends <- o .: "friends"
        uGreeting <- o .: "greeting"
        uFavouriteFruit <- o .: "favoriteFruit"
        return User {..}

instance Aeson.ToJSON EyeColor where
    toJSON ec = Aeson.toJSON $ case ec of
        Green -> "green" :: Text
        Blue -> "blue"
        Brown -> "brown"

    toEncoding ec = Aeson.toEncoding $ case ec of
        Green -> "green" :: Text
        Blue -> "blue"
        Brown -> "brown"

instance Aeson.ToJSON Gender where
    toJSON g = Aeson.toJSON $ case g of
        Male -> "male" :: Text
        Female -> "female"

    toEncoding g = Aeson.toEncoding $ case g of
        Male -> "male" :: Text
        Female -> "female"

instance Aeson.ToJSON Fruit where
    toJSON f = Aeson.toJSON $ case f of
        Apple -> "apple" :: Text
        Banana -> "banana"
        Strawberry -> "strawberry"

    toEncoding f = Aeson.toEncoding $ case f of
        Apple -> "apple" :: Text
        Banana -> "banana"
        Strawberry -> "strawberry"

instance Aeson.ToJSON Friend where
    toJSON Friend {..} = Aeson.object
        [ "id" Aeson..= fId
        , "name" Aeson..= fName
        ]

    toEncoding Friend {..} = Aeson.pairs $
           "id" Aeson..= fId
        <> "name" Aeson..= fName

instance Aeson.ToJSON User where
    toJSON User{..} = Aeson.object
        [ "_id" Aeson..= uId
        , "index" Aeson..= uIndex
        , "guid" Aeson..= uGuid
        , "isActive" Aeson..= uIsActive
        , "balance" Aeson..= uBalance
        , "picture" Aeson..= uPicture
        , "age" Aeson..= uAge
        , "eyeColor" Aeson..= uEyeColor
        , "name" Aeson..= uName
        , "gender" Aeson..= uGender
        , "company" Aeson..= uCompany
        , "email" Aeson..= uEmail
        , "phone" Aeson..= uPhone
        , "address" Aeson..= uAddress
        , "about" Aeson..= uAbout
        , "registered" Aeson..= uRegistered
        , "latitude" Aeson..= uLatitude
        , "longitude" Aeson..= uLongitude
        , "tags" Aeson..= uTags
        , "friends" Aeson..= uFriends
        , "greeting" Aeson..= uGreeting
        , "favoriteFruit" Aeson..= uFavouriteFruit
        ]

    toEncoding User{..} = Aeson.pairs $
          "_id" Aeson..= uId
        <> "index" Aeson..= uIndex
        <> "guid" Aeson..= uGuid
        <> "isActive" Aeson..= uIsActive
        <> "balance" Aeson..= uBalance
        <> "picture" Aeson..= uPicture
        <> "age" Aeson..= uAge
        <> "eyeColor" Aeson..= uEyeColor
        <> "name" Aeson..= uName
        <> "gender" Aeson..= uGender
        <> "company" Aeson..= uCompany
        <> "email" Aeson..= uEmail
        <> "phone" Aeson..= uPhone
        <> "address" Aeson..= uAddress
        <> "about" Aeson..= uAbout
        <> "registered" Aeson..= uRegistered
        <> "latitude" Aeson..= uLatitude
        <> "longitude" Aeson..= uLongitude
        <> "tags" Aeson..= uTags
        <> "friends" Aeson..= uFriends
        <> "greeting" Aeson..= uGreeting
        <> "favoriteFruit" Aeson..= uFavouriteFruit

--- BufferBuilder instances ---

instance Json.ToJson EyeColor where
    toJson ec = Json.toJson $ case ec of
        Green -> "green" :: Text
        Blue -> "blue"
        Brown -> "brown"

instance Json.ToJson Gender where
    toJson g = Json.toJson $ case g of
        Male -> "male" :: Text
        Female -> "female"

instance Json.ToJson Fruit where
    toJson f = Json.toJson $ case f of
        Apple -> "apple" :: Text
        Strawberry -> "strawberry"
        Banana -> "banana"

instance Json.ToJson Friend where
    toJson Friend{..} = Json.toJson $
            "_id" Json..= fId
            <> "name" Json..= fName

instance Json.ToJson User where
    toJson User{..} = Json.toJson $
            "_id"# Json..=# uId
            <> "index"# Json..=# uIndex
            <> "guid"# Json..=# uGuid
            <> "isActive"# Json..=# uIsActive
            <> "balance"# Json..=# uBalance
            <> "picture"# Json..=# uPicture
            <> "age"# Json..=# uAge
            <> "eyeColor"# Json..=# uEyeColor
            <> "name"# Json..=# uName
            <> "gender"# Json..=# uGender
            <> "company"# Json..=# uCompany
            <> "email"# Json..=# uEmail
            <> "phone"# Json..=# uPhone
            <> "address"# Json..=# uAddress
            <> "about"# Json..=# uAbout
            <> "registered"# Json..=# uRegistered
            <> "latitude"# Json..=# uLatitude
            <> "longitude"# Json..=# uLongitude
            <> "tags"# Json..=# uTags
            <> "friends"# Json..=# uFriends
            <> "greeting"# Json..=# uGreeting
            <> "favoriteFruit"# Json..=# uFavouriteFruit

---- json-builder instances ----

instance JB.Value EyeColor where
    toJson ec = JB.toJson $ case ec of
        Green -> "green" :: Text
        Blue -> "blue"
        Brown -> "brown"

instance JB.Value Gender where
    toJson g = JB.toJson $ case g of
        Male -> "male" :: Text
        Female -> "female"

instance JB.Value Fruit where
    toJson f = JB.toJson $ case f of
        Apple -> "apple" :: Text
        Strawberry -> "strawberry"
        Banana -> "banana"

instance JB.Value Friend where
    toJson Friend{..} = JB.toJson $
            ("_id" :: Text) `JB.row` fId
            <> ("name" :: Text) `JB.row` fName

instance JB.Value User where
    toJson User{..} =
        let t :: Text -> Text
            t = id
        in JB.toJson $
               t "_id" `JB.row` uId
            <> t "index" `JB.row` uIndex
            <> t "guid" `JB.row` uGuid
            <> t "isActive" `JB.row` uIsActive
            <> t "balance" `JB.row` uBalance
            <> t "picture" `JB.row` uPicture
            <> t "age" `JB.row` uAge
            <> t "eyeColor" `JB.row` uEyeColor
            <> t "name" `JB.row` uName
            <> t "gender" `JB.row` uGender
            <> t "company" `JB.row` uCompany
            <> t "email" `JB.row` uEmail
            <> t "phone" `JB.row` uPhone
            <> t "address" `JB.row` uAddress
            <> t "about" `JB.row` uAbout
            <> t "registered" `JB.row` uRegistered
            <> t "latitude" `JB.row` uLatitude
            <> t "longitude" `JB.row` uLongitude
            <> t "tags" `JB.row` uTags
            <> t "friends" `JB.row` uFriends
            <> t "greeting" `JB.row` uGreeting
            <> t "favoriteFruit" `JB.row` uFavouriteFruit

benchmarks :: Benchmark
benchmarks = env (load "json-data/buffer-builder.json") $
    \ ~(parsedUserList :: [User]) ->
    bgroup "json-bench" [
      bench "aeson" $ nf Aeson.encode parsedUserList
    , bench "buffer-builder" $ nf Json.encodeJson parsedUserList
    , bench "json-builder" $ nf JB.toJsonLBS parsedUserList
    ]
