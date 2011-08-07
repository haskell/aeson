{-# LANGUAGE OverloadedStrings #-}

-- Simplest example of parsing and encoding JSON with Aeson.

-- Above, we enable OverloadedStrings to allow a literal string (e.g. "name")
-- to be automatically converted to Data.Text.
-- This is useful when using Aeson's functions such as (.:) which expect Text.
-- Without it we'd need to use pack.

import Data.Aeson
import qualified Data.Aeson.Types as T

import Data.Attoparsec (parse, Result(..))
import Data.Text (Text)
import Control.Applicative ((<$>))
import Control.Monad (mzero)
import qualified Data.ByteString.Char8 as BS
-- Aeson's "encode" to JSON generates lazy bytestrings
import qualified Data.ByteString.Lazy.Char8 as BSL

-- In main we'll parse a JSON message into a Msg and display that,
-- then we'll encode a different Msg as JSON, and display it.
main ::IO ()
main = do
  print $ parseMsgFromString exampleJSONMessage
  let reply = Msg "hello Aeson!"
  putStrLn $ "Encoded reply: " ++ BSL.unpack (encode reply)

-- this is the type we'll be converting to and from JSON
data Msg = Msg Text deriving (Show)

-- here's how we should parse JSON and construct a Msg
instance FromJSON Msg where
  parseJSON (Object v) = Msg <$> v .: "message"
  parseJSON _ = mzero

-- here's how we should encode a Msg as JSON
instance ToJSON Msg where
  toJSON (Msg s) = object [ "message" .= s]

-- Here's one way to actually run the parsers.
--
-- Note that we do two parses:
-- once into JSON then one more into our final type.
-- There are a number of choices when dealing with parse failures.
-- Here we've chosen to parse to Maybe Msg, and a Nothing will be returned
-- if parseJSON fails.  (More informative options are available.)
--
-- This should take us (depending on success or failure)
-- from {"message": "hello world"} to Just (Msg "hello world")
--                              or to Nothing
--
-- Note also that we have not checked here that the input has been completely
-- consumed, so:
-- {"message": "hello world"} foo BIG mistake
-- would yield the same successfully translated message!
-- We could look in "rest" for the remainder.
parseMsgFromString :: String -> Maybe Msg
parseMsgFromString s =
  let bs = BS.pack s
  in case parse json bs of
       (Done rest r) -> T.parseMaybe parseJSON r :: Maybe Msg
       _             -> Nothing

-- Here's the example JSON message we're going to try to parse:
-- {"message": "hello world"}
-- It's a JSON object with a single pair, having key 'message', and a string value.
-- It could have more fields and structure, but that's all we're going to parse out of it.
exampleJSONMessage :: String
exampleJSONMessage = "{\"message\":\"hello world\"}"
